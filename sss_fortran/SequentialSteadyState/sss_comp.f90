module sss_comp
use database_io
use globals
use sss
implicit none

   integer, parameter :: NBOX_MAX = 18    ! for now hardcoded max number of boxes for allocating space
   integer :: iter_sss
   integer :: svat_nr_max = 0
   type(t_SequentialSteadyState), allocatable :: svats(:)
   real(kind=hp), allocatable, target    :: rr_array(:)     ! rainfall forcing
   real(kind=hp), allocatable, target    :: ev_array(:)     ! evap forcing
   real(kind=hp), allocatable, target    :: evsoil_array(:) ! actual soil evap
   real(kind=hp), allocatable, target    :: evpond_array(:) ! actual ponding evap
   real(kind=hp), allocatable, target    :: qrun_array(:)   ! runoff
   real(kind=hp), allocatable, target    :: qrot_array(:)   ! crop absorbtion
   real(kind=hp), allocatable, target    :: gwl_array(:)
   real(kind=hp), allocatable, target    :: vsim_array(:)
   real(kind=hp), allocatable, target    :: sc1_array(:)
   real(kind=hp), allocatable, target    :: qmodf_array(:)      
   logical,       target                 :: select_spu(NMAXSPU)      
   integer,       target                 :: soilselect(NMAXSPU)      
   real(kind=hp), allocatable, target    :: phead_array(:,:)
   real(kind=hp), allocatable, target    :: phead_box_array(:,:)
   real(kind=hp), allocatable, target    :: theta_box_array(:,:)      

   real(kind=hp), allocatable, target    :: pond_array(:)      
   integer,       allocatable, target    :: nbox_array(:)   ! number of non-submerged boxes           
   real(kind=hp),              target    :: dtgw(1)
   real(kind=hp),              target    :: tiop(1)

   type(t_databaseSet),    target :: dbset   ! unsa database set
   type(t_downscalingSet), target :: dsset   ! downscaling set

   character(len=*), parameter :: sversion = '0.0.0'
   integer                     :: lun_meteo = 0       ! meteo file handle (test purpose, metaswap style mete_svat.inp) 
   integer                     :: itime = 0           ! timestep number
   integer, parameter          :: spu_max = 370       ! max spu number
   character(len=*), parameter :: dbpath = 'unsa_db'  ! for now hardwired, relative to the cwd 

    type t_meteo
        real(kind=hp) :: time
        real(kind=hp) :: rr
        real(kind=hp) :: ev
    end type t_meteo
    type(t_meteo), target  :: meteo_1, meteo_2
    type(t_meteo), pointer :: meteo_new, meteo_old

!   type t_boxvalues
!      integer                    :: nbox 
!      real(kind=hp), allocatable :: val(:)
!   end type t_boxvalues
!   type(t_boxvalues), allocatable, target :: phead_box_array(:)
!   type(t_boxvalues), allocatable, target :: theta_box_array(:)

contains

      subroutine sss_meteo_update()
      character(len=200) :: line_meteo
          type(t_meteo), pointer :: ptr
          integer :: yr
          do while (tiop(1) >= meteo_new%time)
              ptr => meteo_old
              meteo_old => meteo_new
              meteo_new => ptr
              read(lun_meteo,'(a200)') line_meteo
              read(line_meteo,*) meteo_new%time, yr, meteo_new%rr, meteo_new%ev
          enddo
      end subroutine sss_meteo_update

      subroutine sss_initComponent()
      integer :: lun, ios, svat_nr
      character(len=200) :: line
      open(file='area_svat.inp', status='OLD', newunit=lun, iostat=ios)

!     allocate(select_spu(NMAXSPU))
!     allocate(select_spu(NMAXSPU))

      if (ios==0) then
          svat_nr_max = 0
          do while(.True.)
              read(lun,'(a200)', iostat=ios)  line
              if (ios.ne.0) exit
              if (line(1:1)=='#') cycle
              read(line(1:10),*) svat_nr
              svat_nr_max = max(svat_nr_max,svat_nr)
          enddo
          allocate(rr_array(svat_nr_max))
          allocate(ev_array(svat_nr_max))
          allocate(evsoil_array(svat_nr_max))
          allocate(evpond_array(svat_nr_max))
          allocate(gwl_array(svat_nr_max))
          allocate(vsim_array(svat_nr_max))
          allocate(qrun_array(svat_nr_max))
          allocate(qrot_array(svat_nr_max))
          qrot_array = 0._hp                     ! should be set thru bmi, 
          allocate(sc1_array(svat_nr_max))
          allocate(nbox_array(svat_nr_max))
          allocate(qmodf_array(svat_nr_max))
          allocate(pond_array(svat_nr_max))      
      else
          ! something went wrong opening the area_svat.inp file
          return
      endif
      close(lun)
      end subroutine sss_initComponent

      subroutine sss_initSimulation()
      type(t_sssparam) :: parameters
      integer :: lun, ios, svat_nr, yr, nbox, ierr, ispu
      logical :: exists
      character(len=200) :: line
      character(len=500) :: cwdstr
      iter_sss = 0

      ! tbd, read parameters from a parasim.inp file
      parameters%dtgw = 1._hp              ! groundwater timestep
      parameters%dprz = 1._hp              ! rootzone thickness
      parameters%init_gwl = -3._hp         ! initial groundwater level
      parameters%init_phead = -1.513561_hp ! initial phead

      ! dump cwd to stderr
      call getcwd(cwdstr)
      write(0,*) 'running in CWD = '//trim(cwdstr)

      ! hack: check the cwd for a file select_spu.txt containing a selection of soil physiscal unit numbers
      inquire(file='select_spu.txt',exist=exists)
      if (exists) then
          select_spu = .False.
          open(file='select_spu.txt',status='OLD',newunit=lun)
          ierr = 0
          do while(ierr==0)
              read(lun,'(a200)',iostat=ierr) line
              if (ierr==0 .and. len_trim(line)>0) then
                 read(line,*) ispu
                 if (ispu>=1 .and. ispu<=NMAXSPU) select_spu(ispu) = .True. 
              endif
           enddo
           close(lun)
      endif

      ! Read set of databases. Each database corresponds with a soil physical unit and a rootzone depth
      ! dbpath contains all the netcdf files, dbset is the instance holding the resulting set
      if (.not.dbset%readNCset(dbpath, select_spu)) then
          return        ! reading the database went wrong
      endif
      if (.not.dsset%readNCset(dbpath, select_spu)) then
          return        ! reading the database went wrong
      endif

      ! read area_svat.inp
      open(file='area_svat.inp', status='OLD', newunit=lun, iostat=ios)

      ! prepare svat data structure
      if (ios==0) then
          if (allocated(svats)) deallocate(svats)
          allocate(svats(svat_nr_max))
          if(.not.realloc(phead_array,1,dbset%nbox,1,svat_nr_max)) then
              return ! add exception handling here
          endif

          ios=0
          do while(.True.)
              read(lun,'(a200)', iostat=ios)  line
              if (ios.ne.0) exit
              if (line(1:1)=='#') cycle
              read(line(1:10),*) svat_nr
              read(line(11:20),*) parameters%area            ! area
              read(line(21:28),*) parameters%top             ! elevation
              read(line(37:42),*) parameters%spu             ! soil physical unit number
              read(line(99:110),*) parameters%init_phead     ! initial phead
              read(line(111:122),*) parameters%init_gwl      ! initial gwl
              read(line(123:134),*) parameters%dprz          ! rootzone depth
              read(line(135:142),*) parameters%zmax_ponding  ! ponding reservoir depth
              read(line(143:150),*) parameters%maxinf        ! infiltration rate limit

              svats(svat_nr)%rr => rr_array(svat_nr)
              svats(svat_nr)%ev => ev_array(svat_nr)
              svats(svat_nr)%evsoil => evsoil_array(svat_nr)
              svats(svat_nr)%evpond => evpond_array(svat_nr)
              svats(svat_nr)%gwl => gwl_array(svat_nr)
              svats(svat_nr)%vsim => vsim_array(svat_nr)
              svats(svat_nr)%qrun => qrun_array(svat_nr)
              svats(svat_nr)%sc1 => sc1_array(svat_nr)
              svats(svat_nr)%qmodf => qmodf_array(svat_nr)
              svats(svat_nr)%tiop => tiop(1)
              svats(svat_nr)%dtgw => dtgw(1)
              if (.not.svats(svat_nr)%initialize(parameters,dbset,dsset,phead_array(:,svat_nr))) then
                  !report something went wrong initializing 
                  !the sequential steady state instance
                  return
              endif
          enddo
          close(lun)
          open(file='mete_svat.inp', status='OLD', newunit=lun_meteo, iostat=ios)
          if (ios/=0) then
             write(0,*) 'Problem opening mete_svat.inp for mete input'
             return
          endif 
          read(lun_meteo,'(a200)') line
          read(line,*) meteo_1%time, yr, meteo_1%rr, meteo_1%ev
          read(lun_meteo,'(a200)') line
          read(line,*) meteo_2%time, yr, meteo_2%rr, meteo_2%ev
          meteo_old => meteo_1
          meteo_new => meteo_2
      endif
      end subroutine sss_initSimulation

      subroutine sss_saveFluxes()
      integer :: k
      do k = 1, size(svats)
          call svats(k)%save_fluxes()
      enddo
      end subroutine sss_saveFluxes

      subroutine sss_restoreFluxes()
      integer :: k
      do k = 1, size(svats)
          call svats(k)%restore_fluxes()
      enddo
      end subroutine sss_restoreFluxes

      subroutine sss_prepareTimestep(dt)
      real(kind=hp), intent(in) :: dt
      integer :: k, iy
      real(kind=hp) :: rr, ev, doy, wt
      call sss_meteo_update()    ! update meteo from file mete_svat.inp
      wt = (tiop(1)-meteo_old%time)/(meteo_new%time-meteo_old%time)
      rr_array(:) = (1._hp - wt) * meteo_old%rr + wt * meteo_new%rr
      ev_array(:) = (1._hp - wt) * meteo_old%ev + wt * meteo_new%ev
      call sss_saveFluxes()
      dtgw = dt
      do k = 1, size(svats)
          svats(k)%dtgw = dt
          call svats(k)%prepare(dt,rr_array(k),ev_array(k),qrot_array(k))
          nbox_array(k) = svats(k)%unsa%maxbox
      enddo
      end subroutine sss_prepareTimestep

      subroutine sss_finishTimestep()
      integer :: k
      integer :: b
      real(kind=hp) :: reva(2)
      do k = 1, size(svats)
          call svats(k)%finalize(      &
               gwl_array(k)    * m2cm, & ! in : incoming gwl [cm]
               qmodf_array(k),         & ! out: qmodf [cm/d]
               pond_array(k),          & ! out: ponding depth [cm]
               qrun_array(k),          & ! out: runoff [cm/d]
               theta_box_array(:,k),   & ! out: moisture content by box[-]
               phead_box_array(:,k),   & ! out: the real pressure head by box[-]
               reva)                     ! out: actual soil evaporation [cm] and ponding evaporation 
          evsoil_array(k) = reva(1)   ! todo check the order of these 
          evpond_array(k) = reva(2)
      enddo
      qmodf_array = qmodf_array * m2cm
      pond_array  = pond_array  * m2cm
      qrun_array  = qrun_array  * m2cm
      qmodf_array = qmodf_array * m2cm
      itime = itime + 1
      end subroutine sss_finishTimestep

      subroutine sss_solve()
      integer :: k
      do k = 1, size(svats)
          call svats(k)%do_unsa()
          nbox_array(k) = svats(k)%unsa%maxbox
      enddo
      end subroutine sss_solve

end module sss_comp
