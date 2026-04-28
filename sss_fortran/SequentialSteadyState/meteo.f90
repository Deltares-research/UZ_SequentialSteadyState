module meteo
use globals 
use netcdf
implicit none

integer                     :: lun_meteo = 0       ! meteo file handle (test purpose, metaswap style mete_svat.inp) 

    type t_meteo
        integer       :: lun_meteo ! unit number for meteo
        integer       :: nstn = 0
        integer       :: ntime = 0
        real(kind=hp) :: time
        real(kind=hp), allocatable :: rr(:)
        real(kind=hp), allocatable :: ev(:)
    contains
        procedure, pass :: init => t_meteo_init          ! Read database contents from netCDF4 files
    end type t_meteo
    type(t_meteo), target  :: meteo_1, meteo_2
    type(t_meteo), pointer :: meteo_new, meteo_old

    type t_ncstruct
      integer :: ncid
      integer :: nstn
      integer :: ntime
      integer :: varid_rr
      integer :: varid_ev
      integer :: varid_time
    end type t_ncstruct

    type t_meteo_nc
        integer       :: ncid
        integer       :: nstn = 0
        integer       :: ntime = 0
        real(kind=hp), allocatable :: times(:)
        real(kind=hp), allocatable :: rr(:)
        real(kind=hp), allocatable :: ev(:)
        type(t_meteo_field), pointer :: field0
        type(t_meteo_field), pointer :: field1
    contains
        procedure, pass :: init => t_meteo_nc_init          ! Read database contents from netCDF4 files
    end type t_meteo_nc

    type t_meteo_field
        integer :: ncid
        integer :: nstn
        integer :: varid_rr
        integer :: varid_ev
        integer :: varid_time
        integer :: timelvl
        real(kind=hp), allocatable :: rr(:)
        real(kind=hp), allocatable :: ev(:)
        type(t_ncstruct) :: nc
    contains
        procedure, pass :: init => t_meteo_field_init
        procedure, pass :: update => t_meteo_field_update
    end type t_meteo_field

contains

      subroutine sss_meteo_readblock(meteo)
          type(t_meteo), intent(inout) :: meteo 
          character(len=200) :: line_meteo
          integer :: time, svat, ios
          real(kind=hp) :: rr, ev
          read(lun_meteo,'(a200)') line_meteo
          read(line_meteo,*) time, svat, rr, ev
          meteo%time = time
          meteo%rr(svat) = rr
          meteo%ev(svat) = ev
          do while (.True.)
              read(lun_meteo,'(a200)',iostat=ios) line_meteo
              read(line_meteo,*) time, svat, rr, ev
              if (time/=meteo%time .or. ios<0) then
                  exit
              else
                 meteo%rr(svat) = rr
                 meteo%ev(svat) = ev
              endif
          enddo
          backspace(lun_meteo)
      end subroutine sss_meteo_readblock

      subroutine sss_meteo_init(nstn)
          ! meteo from csv, single time series
          integer, intent(in) :: nstn
          integer :: ios
          open(file='mete_svat.inp', status='OLD', newunit=lun_meteo, iostat=ios)
          if (ios/=0) then
             write(0,*) 'Problem opening mete_svat.inp for mete input'
             return
          endif 
          call meteo_1%init(nstn)
          call meteo_2%init(nstn)
          call sss_meteo_readblock(meteo_1)
          call sss_meteo_readblock(meteo_2)
          meteo_old => meteo_1
          meteo_new => meteo_2
          write(112,'(4E12.4)') meteo_old%time,  meteo_old%rr(1915), meteo_new%time,  meteo_new%rr(1915)    ! RL777 
      end subroutine sss_meteo_init

      subroutine sss_meteo_update(time)
          character(len=200) :: line_meteo
          real(kind=hp), intent(in) :: time
          type(t_meteo), pointer :: ptr
          integer :: yr
          do while (time >= meteo_new%time)
              ptr => meteo_old   
              meteo_old => meteo_new
              meteo_new => ptr
              call sss_meteo_readblock(meteo_new)
          enddo
          write(112,'(5E12.4)') meteo_old%time,  meteo_old%rr(1915), meteo_new%time,  meteo_new%rr(1915), time    ! RL777 
      end subroutine sss_meteo_update

      subroutine t_meteo_init(self, nstn)
          class(t_meteo) :: self
          integer, intent(in) :: nstn
          allocate(self%rr(nstn))
          allocate(self%ev(nstn))
          self%rr=0.0_hp
          self%ev=0.0_hp
      end subroutine t_meteo_init

      subroutine t_meteo_nc_update(self, time)
          class(t_meteo_nc) :: self
          real(kind=hp), intent(in) :: time
          integer :: timelvl
          type(t_meteo_field), pointer :: fieldptr
          timelvl = self%field0%timelvl
          if (time>self%times(timelvl+1)) then      ! search forward in time
             do while(time>self%times(timelvl+1))
                timelvl = timelvl + 1
             enddo
          elseif (time<self%times(timelvl)) then    ! search backward in time
             do while(time<self%times(timelvl))
                timelvl = timelvl - 1
             enddo
          endif
          if ((timelvl>self%field0%timelvl+1) .or. timelvl<self%field0%timelvl-1) then
             call self%field0%update(timelvl)
             call self%field1%update(timelvl+1)
          else
             fieldptr => self%field1
             self%field1 => self%field0
             self%field0 => fieldptr
             if (timelvl==self%field0%timelvl+1) then
                call self%field1%update(timelvl+1)
             elseif (timelvl==self%field0%timelvl-1) then
                call self%field0%update(timelvl)
             endif
          endif
      end subroutine t_meteo_nc_update

      subroutine t_meteo_nc_init(self, filname)
          class(t_meteo_nc) :: self
          character(len=*), intent(in) :: filname
          integer :: ierr, dimid_time, varid_time, dimid_stn
          integer :: nstn, ntime, ncid
          integer :: varid_ev, varid_rr
          type(t_ncstruct) :: ncstruct
          ierr = nf90_open(trim(filname), NF90_NOWRITE, ncid)           ! open meteo stations file in netcdf    
          ierr = nf90_inq_dimid(self%ncid,'time',dimid_time)
          ierr = nf90_inq_dimid(self%ncid,'stn',dimid_stn)
          ierr = nf90_inquire_dimension(ncid, dimid_stn, len=nstn)      ! obtain the number of stations
          ierr = nf90_inquire_dimension(ncid, dimid_time, len=ntime)    ! obtain the number of time levels (unbounded dimension)
          ierr = nf90_inq_varid(ncid,'time',varid_time)                      ! time variable id
          allocate(self%times(self%ntime))
          ierr = nf90_get_var(self%ncid,varid_time,self%times)                    ! retrieve ALL times in the file
          ierr = nf90_inq_varid(ncid,'rr',varid_rr)                     ! rainfall variable id
          ierr = nf90_inq_varid(ncid,'ev',varid_ev)                     ! potential eval variable id
          allocate(self%field0)

          ncstruct%ncid = ncid
          ncstruct%nstn = nstn
          ncstruct%ntime = ntime
          ncstruct%varid_rr = varid_rr
          ncstruct%varid_ev = varid_ev
          ncstruct%varid_time = varid_time

          call self%field0%init(ncstruct, timelvl=0)
          allocate(self%field1)
          call self%field1%init(ncstruct, timelvl=1)
      end subroutine t_meteo_nc_init

      subroutine t_meteo_field_init(self, ncstruct, timelvl )
          class(t_meteo_field) :: self
          type(t_ncstruct), intent(in) :: ncstruct
          integer, intent(in) :: timelvl
          self%nc = ncstruct
          self%timelvl = timelvl
          allocate(self%rr(self%nc%nstn))
          allocate(self%ev(self%nc%nstn))
          call self%update(timelvl)
      end subroutine t_meteo_field_init

      subroutine t_meteo_field_update(self, timelvl)
          class(t_meteo_field) :: self
          integer, intent(in) :: timelvl
          integer :: ierr
          ierr = nf90_get_var(self%nc%ncid, self%nc%varid_rr, self%rr(:), start = (/1,self%timelvl/), count = (/self%nc%nstn,1/))   ! retrieve rainfall for this timelevel
          ierr = nf90_get_var(self%nc%ncid, self%nc%varid_ev, self%ev(:), start = (/1,self%timelvl/), count = (/self%nc%nstn,1/))   ! retrieve evap for this timelevel
      end subroutine t_meteo_field_update

end module meteo