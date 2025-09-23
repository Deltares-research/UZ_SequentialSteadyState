module downscaling
use globals
use database_utils
use database_io
use netcdf
implicit none

    type :: t_downscalingSet
        character(len=:), allocatable            :: unsa_path
        type(t_downscalingptr), allocatable      :: dbs(:)         ! 1D-array of pointers to downscaling data
    contains
        procedure, pass :: readNCset => t_downscalingSet_readNCset ! Read downscaling data from netCDF4 files
    end type t_downscalingSet

    type :: t_downscaling
        integer                    :: nnod = 0
        real(kind=hp), allocatable :: thetatb(:,:,:)               ! node-based soil moisture content
        real(kind=hp), allocatable :: pheadtb(:,:,:)               ! node-based pressure head
        real(kind=hp), allocatable :: z(:)                         ! vertical coordinate of nodes
        real(kind=hp), allocatable :: dz(:)                        ! vertical dimension of nodes
        real(kind=hp)              :: ddpptb
        real(kind=hp)              :: ddgwtb
        real(kind=hp)              :: dpczsl
        real(kind=hp)              :: dprzsl
    contains
        procedure, pass :: readNC => t_downscaling_readNC          ! Read database contents from netCDF4 files
    end type t_downscaling

    type :: t_downscalingPtr
        type (t_downscaling), pointer :: ptr => null()
    end type t_downscalingPtr
    
contains

    function t_downscaling_readNC(ds,ncid) result (success)
        logical :: success
        class(t_downscaling), intent(inout) :: ds
        integer,              intent(in)    :: ncid   ! netcdf file handle
    
        character(len=:), allocatable :: filname
        character(len=3) dum
        integer :: dimid_node
        integer :: varid_theta, varid_phead, varid_dz
        integer :: ios, iline
        integer :: lun, ir, ip, ig, sl, rz, sl_tgt, igdcmn, igdcmx
        integer :: ierr, inod, nnod, nuip, nlip, nuig, nlig, nxlig, nxuig
        logical :: exists
    
        success = .False.
        ierr = nf90_inq_dimid(ncid,'node',dimid_node)
        ierr = nf90_inquire_dimension(ncid, dimid_node, len=nnod)
        ds%nnod = nnod
        ierr = nf90_get_att(ncid, NF90_GLOBAL, 'igdcmn', igdcmn)
        ierr = nf90_get_att(ncid, NF90_GLOBAL, 'igdcmx', igdcmx)
        ierr = nf90_get_att(ncid, NF90_GLOBAL, 'ddpptb', ds%ddpptb)
        ierr = nf90_get_att(ncid, NF90_GLOBAL, 'ddgwtb', ds%ddgwtb)
        ierr = nf90_get_att(ncid, NF90_GLOBAL, 'dpczsl', ds%dpczsl)
        ierr = nf90_get_att(ncid, NF90_GLOBAL, 'dprzsl', ds%dprzsl)
        ierr = nf90_get_att(ncid, NF90_GLOBAL, 'nuip', nuip)
        ierr = nf90_get_att(ncid, NF90_GLOBAL, 'nlip', nlip)
        ierr = nf90_get_att(ncid, NF90_GLOBAL, 'nlig', nlig)
        ierr = nf90_get_att(ncid, NF90_GLOBAL, 'nuig', nuig)
        ierr = nf90_get_att(ncid, NF90_GLOBAL, 'nxlig', nxlig)
        ierr = nf90_get_att(ncid, NF90_GLOBAL, 'nxuig', nxuig)
    
        ! allocate arrays in db
        success = realloc(ds%thetatb, nxlig-1, nuig, nlip, nuip, 1, nnod)
        success = realloc(ds%pheadtb, nxlig-1, nuig, nlip, nuip, 1, nnod)
        success = realloc(ds%z,  0, nnod)
        success = realloc(ds%dz, 1, nnod)

        ! initialize contents of all tables (for missing soiltypes)
        ds%thetatb = DMISS
    
        ! inquire variable id
        ierr = nf90_inq_varid(ncid, 'thetatb', varid_theta)
        ierr = nf90_inq_varid(ncid, 'pheadtb', varid_phead)
        ierr = nf90_inq_varid(ncid, 'dz_key', varid_dz)

        ! read variable data
        ierr = nf90_get_var(ncid, varid_theta, ds%thetatb(:,:,:))
        ierr = nf90_get_var(ncid, varid_phead, ds%pheadtb(:,:,:))
        ierr = nf90_get_var(ncid, varid_dz, ds%dz(:))
        ds%z(0)=0.d0
        do inod=1,nnod
           ds%z(inod)=ds%z(inod-1)-ds%dz(inod)
        enddo

        ! store z and dz of nodes
        success = .True.
    end function t_downscaling_readNC

    function t_downscalingSet_readNCset(dsset, unsa_path, select_spu) result (success)
        logical :: success
        class(t_downscalingSet), intent(inout) :: dsset
        character(*),            intent(in)    :: unsa_path
        logical, optional,       intent(in)    :: select_spu(:) ! logical array of selected soil types
    
        character(len=:), allocatable :: filname
        character(len=60) :: regel
        character(len=3) dum
        integer :: ncid, lun
        integer :: ios, ilines
        integer :: spu, rz, nsl
        integer :: dimid_rz, varid_rz
        integer :: ierr
        logical :: exists
        class(t_downscaling), pointer :: ds => null()
        character(len=200) :: cwd
        real(kind=hp) :: drz
    
        success = .False.
        dsset%unsa_path = unsa_path

        allocate(dsset%dbs(NMAXSPU))

        ! read table contents from files
        do spu=1, NMAXSPU
           if (present(select_spu)) then
              if (.not.select_spu(spu)) then
                 cycle
              endif
           endif
           write(dum,'(i3.3)') spu
           filname=trim(dsset%unsa_path)//'/nodes_'//dum//'.nc'
           ierr = nf90_open(trim(filname), NF90_NOWRITE, ncid)
           if (ierr.eq.NF90_NOERR) then
              write(0,*) 'reading '//filname//' ...' 
              ! create downscaling instance
              allocate(ds)                           ! note: test this works 
              if (.not.ds%readNC(ncid)) then
                  return           ! problem occurred reading this particular database
              endif
              dsset%dbs(spu)%ptr => ds
           endif
           ierr = nf90_close(ncid) ! close open netcdf file
        enddo ! soiltypes on nc-files

        success = .True.
    end function t_downscalingSet_readNCset

end module downscaling
