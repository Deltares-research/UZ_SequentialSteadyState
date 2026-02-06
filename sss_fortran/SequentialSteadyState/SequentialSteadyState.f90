module sss
use globals
use database_io
use unsaturated_zone
use ponding
implicit none

    type :: t_sssparam
        integer                   :: spu        ! soiltype number
        real(kind=hp)             :: dtgw       ! groundwater timestep
        real(kind=hp)             :: dprz       ! rootzone thickness
        real(kind=hp)             :: area       ! svat area
        real(kind=hp)             :: top        ! surface elevation
        real(kind=hp)             :: init_gwl   ! initial groundwater level
        real(kind=hp)             :: init_phead ! initial phead
        real(kind=hp)             :: zmax_ponding ! zmax depth for ponding reservoir
        real(kind=hp)             :: maxinf       ! limiting infiltration rate
        integer(kind=hp)          :: niter        ! standalone test number of iterations
    end type t_sssparam

    type :: t_SequentialSteadyState
        real(kind=hp), pointer        :: dtgw => null()  ! groundwater timestep
        real(kind=hp), pointer        :: tiop => null()  ! groundwater time (for reading only)
        real(kind=hp), pointer        :: rr => null()    ! (pointer to) rainfall forcing
        real(kind=hp), pointer        :: ev => null()    ! (pointer to) potential evaporation forcing
        real(kind=hp), pointer        :: evpond => null()! evap from ponding
        real(kind=hp), pointer        :: evsoil => null()! evap from soil
        real(kind=hp), pointer        :: gwl  => null()  ! groundwater level, [IN]
        real(kind=hp)                 :: gwl0            ! old groundwater level
        real(kind=hp)                 :: ds              ! storage change
        real(kind=hp), pointer        :: sc1  => null()  ! storage coefficient, [OUT]  
        real(kind=hp), pointer        :: vsim => null()  ! rechange flux to saturated zone, [OUT]  
        real(kind=hp), pointer        :: qmodf => null() ! qmodf [INOUT]
        real(kind=hp), pointer        :: qrun => null()  ! qrun [OUT] excess runoff 
        real(kind=hp)                 :: qrch            ! recharge flux
        real(kind=hp)                 :: qrot            ! crop extraction
        integer, allocatable          :: nod2box(:)      ! mapping of downscaling nodes to boxes
        type(t_unsa)                  :: unsa            ! unsaturated zone instance belonging to this model
        type(t_ponding)               :: ponding         ! ponding instance       
        type(t_soil)                  :: soil            ! soil instance for evaporation       
        real(kind=hp), allocatable    :: qmv0(:)         ! fluxes saved prior to the iteration loop

        integer       :: spu   ! soil-physid0cal-unit 
        real(kind=hp) :: area  ! svat area
    contains
        procedure, pass :: do_unsa        => t_SequentialSteadyState_do_unsa
        procedure, pass :: save_fluxes    => t_SequentialSteadyState_save_fluxes
        procedure, pass :: restore_fluxes => t_SequentialSteadyState_restore_fluxes

        procedure, pass :: initialize     => t_SequentialSteadyState_initialize          ! initialize megaswap simulation instance SVAT
        procedure, pass :: prepare        => t_SequentialSteadyState_prepare             ! prepare timestep: meteo, ponding, evap etc. top system
        procedure, pass :: calc           => t_SequentialSteadyState_calc                ! update profile given change groundwater level (iterations)
        procedure, pass :: finalize       => t_SequentialSteadyState_finalize            ! process changed qmodf
        procedure, pass :: downscale      => t_SequentialSteadyState_downscale           ! downscale pressure heads and moistur content to node level
    end type t_SequentialSteadyState
    
   type, extends(t_SequentialSteadyState) :: t_SequentialSteadyStateSingle
   contains
        procedure, pass :: initialize  => t_SequentialSteadyStateSingle_initialize
        procedure, pass :: cleanup     => t_SequentialSteadyStateSingle_cleanup
   end type t_SequentialSteadyStateSingle

contains

  subroutine nodes2boxes(dz_key, hbotb, nod2box)
     real(kind=hp), intent(in) :: dz_key(:)    ! node z-level relative to top
     real(kind=hp), intent(in) :: hbotb(0:)    ! lower extend of box relative to top, assumed 0-based
     integer, intent(out)     :: nod2box(:)   ! box number for each node
     real(kind=hp) :: z         ! node z-level relative to top
     integer :: ibox, nbox, inode, nnode
     nnode = min(ubound(dz_key,dim=1), ubound(nod2box,dim=1))
     nbox  = ubound(hbotb,dim=1)

     ibox = 1         ! boxnr 0 not allowed here, skip it
     z = 0.0_hp
     do inode=1,nnode
        do while ((z<hbotb(ibox)) .and. (ibox<nbox))
           ibox = ibox + 1
        enddo
        nod2box(inode) = ibox
        z = z - dz_key(inode)
     enddo
  end subroutine nodes2boxes

  function t_SequentialSteadyState_initialize(sss, parameters, dbset, dsset, pheadptr) result (success)
        logical :: success
        class(t_SequentialSteadyState), target, &
                                           intent(inout) :: sss
        type (t_sssparam),                 intent(in)    :: parameters
        type (t_databaseSet),     pointer, intent(in)    :: dbset
        type (t_downscalingSet),  pointer, intent(in), optional :: dsset
        real (kind=hp),           pointer, intent(in), optional :: pheadptr(:)
        type (t_database),     pointer :: dbptr
        type (t_downscaling),  pointer :: dsptr
        integer :: ierr, nbox
        success = .False. 

        ! initialize unsaturated zone
        dbptr => dbset%getDbPtr(parameters%spu,parameters%dprz)     
        if (present(dsset)) then
            dsptr => dsset%dbs(parameters%spu)%ptr
        endif

        sss%area = parameters%area
        sss%dtgw = parameters%dtgw
        nbox = ubound(dbptr%hbotb,dim=1)
        if (present(pheadptr)) then
            sss%unsa%phead => pheadptr
        else
            allocate(sss%unsa%phead(nbox))
        endif
        sss%gwl = parameters%init_gwl / m2cm
        sss%gwl0 = parameters%init_gwl / m2cm
        if (.not.sss%unsa%initialize(parameters%init_phead, sss%gwl, dbptr, dsptr)) return
        if (allocated(sss%qmv0)) deallocate (sss%qmv0)
        allocate(sss%qmv0(nbox))
        sss%unsa%top = parameters%top

        sss%ponding%zmax                    = parameters%zmax_ponding    ! zmax depth for ponding reservoir
        sss%ponding%area                    = parameters%area            ! svat area
        sss%ponding%max_infiltration_rate   = parameters%maxinf          ! limiting infiltration rate
        sss%ponding%dt                      => sss%dtgw                  ! timestep

        call sss%soil%reset()
        sss%soil%dt => sss%dtgw
        sss%unsa%dt => sss%dtgw

        ! change of storage in unsaturated zone after unsaturated_zone.update()
        sss%ds = 0._hp
        success = .True. 

        if (present(dsset)) then
           dsptr => dsset%dbs(parameters%spu)%ptr
           ! make mapping of box numbers and node numbers
           if (allocated(sss%nod2box)) deallocate(sss%nod2box)
           allocate(sss%nod2box(dsptr%nnod))
           call nodes2boxes(dsptr%dz, sss%unsa%unsa_db%hbotb, sss%nod2box)
        endif
    end function t_SequentialSteadyState_initialize

    subroutine t_SequentialSteadyState_do_unsa(sss)
        class(t_SequentialSteadyState), intent(inout) :: sss
        real(kind=hp) :: gam
        sss%ds = sss%unsa%update(sss%qrch, sss%gwl)
        sss%vsim = (sss%qrch*sss%dtgw - sss%ds) * sss%area
        gam = sss%unsa%gwl2gamma(sss%gwl)
        if (sss%gwl > sss%unsa%top) then
           sss%sc1 = 1._hp
        else
           sss%sc1 = sss%unsa%get_sc1(gam)
        endif
        call sss%restore_fluxes()
    end subroutine t_SequentialSteadyState_do_unsa

    subroutine t_SequentialSteadyState_save_fluxes(sss)
        class(t_SequentialSteadyState), intent(inout) :: sss
        sss%qmv0(:) = sss%unsa%qmv(:)
    end subroutine t_SequentialSteadyState_save_fluxes

    subroutine t_SequentialSteadyState_restore_fluxes(sss)
        class(t_SequentialSteadyState), intent(inout) :: sss
        sss%unsa%qmv(:) = sss%qmv0(:)
    end subroutine t_SequentialSteadyState_restore_fluxes

    function t_SequentialSteadyStateSingle_initialize(sss, parameters, dbset, dsset, pheadptr) result (success)
        logical :: success
        class(t_SequentialSteadyStateSingle), target, &
                                        intent(inout) :: sss
        type (t_sssparam),                 intent(in) :: parameters
        type (t_databaseSet),     pointer, intent(in) :: dbset
        type (t_downscalingSet),  pointer, intent(in), optional    :: dsset
        real (kind=hp),           pointer, intent(in), optional    :: pheadptr(:)
        success = .False. 
        allocate(sss%rr, sss%ev, sss%evpond, sss%evsoil)
        allocate(sss%gwl, sss%sc1, sss%vsim, sss%qmodf, sss%qrun)
        if (.not.sss%t_SequentialSteadyState%initialize(parameters, dbset, dsset)) then
            ! add error handling
            return
        endif
        success = .True. 
    end function t_SequentialSteadyStateSingle_initialize

    subroutine t_SequentialSteadyStateSingle_cleanup(sss)
        class(t_SequentialSteadyStateSingle), intent(inout) :: sss
        deallocate(sss%rr, sss%ev, sss%evpond, sss%evsoil)
        deallocate(sss%gwl, sss%sc1, sss%vsim, sss%qmodf, sss%qrun)
    end subroutine t_SequentialSteadyStateSingle_cleanup

    subroutine t_SequentialSteadyState_prepare(sss,dt,nraidt,peva,qrot)
        class(t_SequentialSteadyState), intent(inout) :: sss
        real(kind=hp), intent(IN)  :: dt      ! time step [d] 
        real(kind=hp), intent(IN)  :: nraidt  ! net rainfall depth [cm]
        real(kind=hp), intent(IN)  :: peva    ! potential evap depth [cm/d]
        real(kind=hp), intent(IN)  :: qrot    ! crop absorbtion (sink term) [cm/d]
        sss%dtgw = dt
        sss%rr = nraidt / m2cm
        sss%ev = peva / m2cm
        call sss%ponding%addPrecip(sss%rr)
        sss%qrch = sss%ponding%getInfiltration(sss%gwl)
        call sss%soil%update(sss%qrch, sss%ev, sss%dtgw)
        sss%evsoil = sss%soil%getActualEvap()
        if ((sss%ponding%volume > 0._hp) .or. (sss%gwl > sss%unsa%top)) then
            call sss%soil%reset()
            sss%evsoil=0._hp
        endif
        sss%qrot = qrot / m2cm
        sss%qrch = sss%qrch - sss%qrot - sss%evsoil       ! reduce the infiltration by the imposed crop consumption and soil evaporation
        call sss%do_unsa()
    end subroutine t_SequentialSteadyState_prepare

    subroutine t_SequentialSteadyState_calc(sss,gwl,h,qsim,sc1)
        class(t_SequentialSteadyState), intent(inout) :: sss
        real(kind=hp), intent(IN)  :: gwl     ! groundwater level [cm]
        real(kind=hp), intent(OUT) :: h(:)    ! pressure head [cm]
        real(kind=hp), intent(OUT) :: qsim    ! recharge flux to groundwater [cm/d]
        real(kind=hp), intent(OUT) :: sc1     ! storage coefficient [cm/d]
        sss%gwl = gwl
        call sss%do_unsa()
        h(:) = sss%unsa%phead(:) * m2cm
        qsim = sss%vsim/sss%area * m2cm
        sc1 = sss%sc1
    end subroutine t_SequentialSteadyState_calc

    subroutine t_SequentialSteadyState_finalize(sss,gwl,pond,qrun,qmodf,sv_box,phead_box,reva,submerged)
        class(t_SequentialSteadyState), intent(inout) :: sss
        real(kind=hp),    intent(IN)  :: gwl           ! incoming gwl [cm]
        real(kind=hp),    intent(OUT) :: pond          ! ponding depth [cm]
        real(kind=hp),    intent(OUT) :: qrun          ! runoff [cm/d]
        real(kind=hp),    intent(OUT) :: qmodf         ! qmodf [cm/d]
!       real(kind=hp),    intent(OUT) :: theta_box(:)  ! moisture content by box[-]
        real(kind=hp),    intent(OUT) :: sv_box(:)     ! storage by box[-]
        real(kind=hp),    intent(OUT) :: phead_box(:)  ! tahe real pressure head by box[-]
        real(kind=hp),    intent(OUT) :: reva(2)       ! actual soil evaporation [cm] and ponding evaporation
        logical,          intent(OUT), optional :: submerged(:)  ! True/False mask of submerged boxes (True=box is submerged)
        real(kind=hp) :: th1, th2

        sss%gwl = gwl / m2cm
        sss%qmodf = (sss%gwl - sss%gwl0) * sss%sc1 - (sss%vsim/sss%area) 
        call sss%unsa%finalize_timestep(sss%gwl,sss%qmodf)
        sss%qrun = sss%ponding%getRunoff()
        pond = sss%ponding%stage * m2cm
        sss%evpond = sss%ponding%getPondingEvap(sss%ev)
        qrun = sss%qrun * m2cm
        reva(1) = sss%soil%evac * m2cm          ! actual soil evaap
        reva(2) = sss%evpond * m2cm             ! actual ponding evap
        qmodf = sss%qmodf * m2cm
        call sss%save_fluxes()
        sss%gwl0 = sss%gwl
        call sss%unsa%get_storage(sv_box)
        call sss%unsa%get_phead(phead_box)
    end subroutine t_SequentialSteadyState_finalize

    subroutine t_SequentialSteadyState_downscale(sss, thnode, phnode)
        class(t_SequentialSteadyState), intent(inout) :: sss
        real(kind=hp),    intent(OUT) :: thnode(:)  ! moisture content by node[-]
        real(kind=hp),    intent(OUT) :: phnode(:)  ! moisture content by node[-]
        real(kind=hp) :: phi, gam
        integer       :: bn, lgam, lphi, inode
        lgam = lbound(sss%unsa%unsa_db%svtb, dim=2) ! assumed to be the same dimensions in the node tables 
        lphi = lbound(sss%unsa%unsa_db%svtb, dim=3)

        do inode = 1, sss%unsa%nnod
           bn  = sss%nod2box(inode)
           phi = sss%unsa%phi(bn)
           gam = sss%unsa%gam
           thnode(inode) = bilinear(sss%unsa%downscaling_db%thetatb(:,:,inode),gam,phi,lgam,lphi)
           phnode(inode) = bilinear(sss%unsa%downscaling_db%pheadtb(:,:,inode),gam,phi,lgam,lphi)
        enddo
    end subroutine t_SequentialSteadyState_downscale

end module sss
