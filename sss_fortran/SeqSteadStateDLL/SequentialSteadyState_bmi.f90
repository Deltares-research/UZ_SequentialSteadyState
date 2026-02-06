!  SequentialSteadyState_bmi.f90 
!
!  FUNCTIONS/SUBROUTINES exported from MetaSWAPDLL.dll:
!  MetaSWAPDLL - subroutine 
!
module sssbmi
   use bmif, only: BMI_SUCCESS, BMI_FAILURE
   use iso_c_binding, only: c_int, c_char, c_double, C_NULL_CHAR, c_loc, c_ptr
   use sss_comp

   implicit none

   ! Define global constants  

   integer(c_int), BIND(C, name="MAXSTRLEN") :: MAXSTRLEN = 1000
   !DEC$ ATTRIBUTES DLLEXPORT :: MAXSTRLEN

   double precision, TARGET :: current_time         ! Simulation time
   integer :: iter                                  ! iteratio number ? (see if this can be removed)

   integer(c_int), bind(C, name="BMI_LENVARTYPE") :: BMI_LENVARTYPE = 1000
   !DEC$ ATTRIBUTES DLLEXPORT :: BMI_LENVARTYPE

   ! Has no meaning in MetaSWAP, but required in XMIPi
   integer(c_int), bind(C, name="BMI_LENGRIDTYPE") :: BMI_LENGRIDTYPE = 0 
   !DEC$ ATTRIBUTES DLLEXPORT :: BMI_LENGRIDTYPE
  
   integer(c_int), bind(C, name="BMI_LENVARADDRESS") :: BMI_LENVARADDRESS = 0
   !DEC$ ATTRIBUTES DLLEXPORT :: BMI_LENVARADDRESS
   
   integer(c_int), bind(C, name="BMI_LENVERSION") :: BMI_LENVERSION = 256
   !DIR$ ATTRIBUTES DLLEXPORT :: BMI_LENVERSION


contains  
  
  ! returns sversion from simvar module 
  function bmi_get_version(sss_version) result(bmi_status) bind(C, name="get_version")
    !DIR$ ATTRIBUTES DLLEXPORT :: bmi_get_version  
    character(kind=c_char), intent(out) :: sss_version(BMI_LENVERSION)
    integer(kind=c_int) :: bmi_status    
    
    sss_version = string_to_char_array(sversion, len_trim(sversion))
    bmi_status = BMI_SUCCESS
  end function bmi_get_version

   function bmi_initialize() result(bmi_status) bind(C, name="initialize")
      !DEC$ ATTRIBUTES DLLEXPORT :: bmi_initialize
      integer(kind=c_int)      :: bmi_status
      soilselect = 1        ! temporary: set all soil types active
      select_spu = (soilselect>0)
      call sss_initComponent()
      call sss_initSimulation()
      bmi_status = BMI_SUCCESS
   end function bmi_initialize

   function bmi_prepare_timestep(dt) result(bmi_status) bind(C, name="prepare_time_step")
   !DEC$ ATTRIBUTES DLLEXPORT :: bmi_prepare_timestep
      real(kind=c_double)     :: dt
      integer(kind=c_int)     :: bmi_status
      call sss_prepareTimestep(dt)
      bmi_status = BMI_SUCCESS
   end function bmi_prepare_timestep

   function bmi_solve() result(bmi_status) bind(C, name="solve")
   !DEC$ ATTRIBUTES DLLEXPORT :: bmi_solve
      real(kind=c_double)     :: dt
      integer(kind=c_int)     :: bmi_status
      call sss_solve()
      bmi_status = BMI_SUCCESS
   end function bmi_solve

   function bmi_finalize_timestep() result(bmi_status) bind(C, name="finalize_time_step")
   !DEC$ ATTRIBUTES DLLEXPORT :: bmi_finalize_timestep
      integer(kind=c_int)     :: bmi_status
      real(kind=c_double)     :: time 
      call sss_finishTimestep()   ! time should be current time
      bmi_status = BMI_SUCCESS
   end function bmi_finalize_timestep

   function bmi_prepare_solve() result(bmi_status) bind(C, name="prepare_solve")
   !DEC$ ATTRIBUTES DLLEXPORT :: bmi_prepare_solve
      integer(kind=c_int)      :: bmi_status
      bmi_status = BMI_SUCCESS
   end function bmi_prepare_solve

   function bmi_finalize_solve(subcomponent_idx) result(bmi_status) bind(C, name="finalize_solve")
   !DEC$ ATTRIBUTES DLLEXPORT :: bmi_finalize_solve
      integer(kind=c_int), intent(in) :: subcomponent_idx
      integer(kind=c_int)      :: bmi_status
      bmi_status = BMI_SUCCESS
   end function bmi_finalize_solve
     
   function bmi_update() result(bmi_status) bind(C, name="update")
   !DEC$ ATTRIBUTES DLLEXPORT :: bmi_update
      integer(kind=c_int)      :: bmi_status
      bmi_status = BMI_FAILURE
   end function bmi_update

   function bmi_finalize() result(bmi_status) bind(C, name="finalize")
   !DEC$ ATTRIBUTES DLLEXPORT :: bmi_finalize
      integer(kind=c_int)      :: bmi_status
      bmi_status = BMI_SUCCESS
   end function bmi_finalize

   function bmi_saveState() result(bmi_status) bind(C, name="saveState")
   !DEC$ ATTRIBUTES DLLEXPORT :: bmi_saveState
      integer(kind=c_int)        :: bmi_status
      integer*4                  :: error
      bmi_status = BMI_SUCCESS
   end function bmi_saveState

   function bmi_restoreState() result(bmi_status) bind(C, name="restoreState")
   !DEC$ ATTRIBUTES DLLEXPORT :: bmi_restoreState
      integer(kind=c_int)        :: bmi_status
      integer*4                  :: error
      bmi_status = BMI_SUCCESS
   end function bmi_restoreState

   function get_value_ptr(c_var_name, x) result (bmi_status) bind(C, name="get_value_ptr")
   !DEC$ ATTRIBUTES DLLEXPORT :: get_value_ptr
      integer(kind=c_int)                :: bmi_status
      character(kind=c_char), intent(in) :: c_var_name(*) !< Variable name. May be slash separated string "name/item/field": then get_compound_field is called.
      type(c_ptr), intent(inout)         :: x
      character(len=strlen(c_var_name))  :: var_name      ! The fortran name of the attribute name
      var_name = char_array_to_string(c_var_name, strlen(c_var_name))
      select case(var_name)
!     case ()
!        bmi_status = get_value_ptr_float(c_var_name, x)
      case ("lvgw","lvgwmodf","qmodf","vsim","qrun","qrot","evsoil","evpond","sc1sim","phead","dtgw","tiop")
         bmi_status = get_value_ptr_double(c_var_name, x)
      case ("nbox","soilselect")
         bmi_status = get_value_ptr_int(c_var_name, x)
      case default
         bmi_status = BMI_FAILURE
      end select
      return
   end function get_value_ptr

   function get_value_ptr_float(c_var_name, x) result (bmi_status) bind(C, name="get_value_ptr_float")
   !DEC$ ATTRIBUTES DLLEXPORT :: get_value_ptr_float
      integer(kind=c_int)                :: bmi_status
      character(kind=c_char), intent(in) :: c_var_name(*) !< Variable name. May be slash separated string "name/item/field": then get_compound_field is called.
      type(c_ptr), intent(inout)         :: x
      character(len=strlen(c_var_name))  :: var_name      ! The fortran name of the attribute name
      var_name = char_array_to_string(c_var_name, strlen(c_var_name))
      select case(var_name)
      case default
         bmi_status = BMI_FAILURE
         return
      end select
      bmi_status = BMI_SUCCESS
   end function get_value_ptr_float

   function get_value_ptr_double(c_var_name, x) result(bmi_status) bind(C, name="get_value_ptr_double")
   !DEC$ ATTRIBUTES DLLEXPORT :: get_value_ptr_double
      integer(kind=c_int)                :: bmi_status
      character(kind=c_char), intent(in) :: c_var_name(*) !< Variable name. May be slash separated string "name/item/field": then get_compound_field is called.
      type(c_ptr), intent(inout)         :: x
      character(len=strlen(c_var_name))  :: var_name      !< The fortran name of the attribute name
      var_name = char_array_to_string(c_var_name, strlen(c_var_name))
      select case(var_name)
      case ("lvgw","lvgwmodf")     ! groundwaterlevel inside MetaSWAP (m)
            x = c_loc(gwl_array)
      case ("phead")               ! pressure head profile inside MetaSWAP (m)
            x = c_loc(phead_array)
      case ("qmodf")               ! modflow contribution to the flux (m3/d)
            x = c_loc(qmodf_array)
      case ("nbox")                ! number of non-submerged boxes for an svat
            x = c_loc(nbox_array)
      case ("vsim")                ! ModSimUnsaturatedZoneVolume (m3)
            x = c_loc(vsim_array)
      case ("qrun")                ! Excess runoff (m3)
            x = c_loc(qrun_array)
      case ("qrot")                ! crop absorbtion (m3) sink term
            x = c_loc(qrot_array)
      case ("evsoil")              ! Evap from soil
            x = c_loc(evsoil_array)
      case ("evpond")              ! Evap from ponding
            x = c_loc(evpond_array)
      case ("sc1sim")              ! StorageFactor (m3/m)
            x = c_loc(sc1_array)
      case ("dtgw")                ! Surface water timestep
            x = c_loc(dtgw)
      case ("tiop")                ! Surface water absolute time
            x = c_loc(tiop)
      case default
          bmi_status = BMI_FAILURE
          return
      end select
      bmi_status = BMI_SUCCESS
   end function get_value_ptr_double
  
   function get_value_ptr_int(c_var_name, x) result (bmi_status) bind(C, name="get_value_ptr_int")
   !DEC$ ATTRIBUTES DLLEXPORT :: get_value_ptr_int
      integer(kind=c_int)                :: bmi_status
      character(kind=c_char), intent(in) :: c_var_name(*) !< Variable name. May be slash separated string "name/item/field": then get_compound_field is called.
      type(c_ptr), intent(inout)         :: x
      character(len=strlen(c_var_name))  :: var_name       ! The fortran name of the attribute name
      var_name = char_array_to_string(c_var_name, strlen(c_var_name))
      select case(var_name)
      case ("soilselect")              ! number of non-submerged boxes
            x = c_loc(soilselect)
      case ("nbox")              ! number of non-submerged boxes
            x = c_loc(nbox_array)
      case default
          bmi_status = BMI_FAILURE
          return
      end select
      bmi_status = BMI_SUCCESS
   end function get_value_ptr_int
  
   function get_var_type(c_var_name, c_var_type) result(bmi_status) bind(C, name="get_var_type")
   !DEC$ ATTRIBUTES DLLEXPORT :: get_var_type
      integer(kind=c_int)                  :: bmi_status
      character (kind=c_char), intent(in)  :: c_var_name(*)
      character (kind=c_char), intent(out) :: c_var_type(MAXSTRLEN)
      character(len=MAXSTRLEN) :: type_name, var_name
      integer :: lentype

      var_name = char_array_to_string(c_var_name, strlen(c_var_name))
      select case(var_name)
      case ("hgwmod")              ! Heads (m)
          type_name = 'double'
      case ("lvgw","lvgwmodf")     ! Waterlevels inside MetaSWAP (m)
          type_name = 'double'
      case ("phead")               ! pressure heads for MetaSWAP boxes(m)
          type_name = 'double'
      case ("qmodf")               ! Modflow contribution to flux (m3)
          type_name = 'double'
      case ("nbox")                ! number of non-submerged boxes
          type_name = 'integer'
      case ("soilselect")          ! selection of soil numbers
          type_name = 'integer'
      case ("vsim")                ! ModSimUnsaturatedZoneFlux (m3)
          type_name = 'double'
      case ("qrun")                ! Excess runoff (m3)
          type_name = 'double'
      case ("qrot")                ! crop absorbtion (m3) sink term
          type_name = 'double'
      case ("evsoil")              ! Evap from soil
          type_name = 'double'
      case ("evpond")              ! Evap from ponding
          type_name = 'double'
      case ("sc1sim")              ! StorageFactor (m3/m)
          type_name = 'double'
      case ("dtgw","tiop")         ! Surface water timestep and absolute time
          type_name = 'double'
      case default
          bmi_status = BMI_FAILURE
          return
      end select
      lentype = len_trim(type_name)
      c_var_type(1:lentype+1) = string_to_char_array(type_name,lentype)
      bmi_status = BMI_SUCCESS
   end function get_var_type
  

   function get_var_rank(c_var_name, c_var_rank) result(bmi_status) bind(C, name="get_var_rank")
   !DEC$ ATTRIBUTES DLLEXPORT :: get_var_rank
      integer(kind=c_int)                :: bmi_status
      character(kind=c_char), intent(in) :: c_var_name(*) !< Variable name. May be slash separated string "name/item/field": then get_compound_field is called.
      character(len=strlen(c_var_name))  :: var_name       ! The fortran name of the attribute name
      integer(kind=c_int), intent(out)   :: c_var_rank
      var_name = char_array_to_string(c_var_name, strlen(c_var_name))
      select case(var_name)
      case ("hgwmod")              ! Heads (m)
          c_var_rank = 1
      case ("lvgw","lvgwmodf")     ! Waterlevels inside MetaSWAP (m)
          c_var_rank = 1
      case ("phead")               ! pressure heads for MetaSWAP boxes(m)
          c_var_rank = 2
      case ("qmodf")               ! 
          c_var_rank = 1 
      case ("nbox")                ! 
          c_var_rank = 1 
      case ("soilselect")          ! 
          c_var_rank = 1 
      case ("vsim")                ! ModSimUnsaturatedZoneFlux (m3)
          c_var_rank = 1 
      case ("qrun")                ! Excess runoff (m3)
          c_var_rank = 1 
      case ("qrot")                ! crop absorbtion (m3) sink term
          c_var_rank = 1 
      case ("evsoil")              ! Evap from soil
          c_var_rank = 1 
      case ("evpond")              ! Evap from ponding
          c_var_rank = 1 
      case ("sc1sim")              ! StorageFactor (m3/m)
          c_var_rank = 1
      case ("dtgw","tiop")         ! Surface water timestep and absolute time0
          c_var_rank = 1
      case default
          bmi_status = BMI_FAILURE
          return
      end select
      bmi_status = BMI_SUCCESS
   end function get_var_rank
   
 
   function get_var_shape(c_var_name, c_var_shape) result(bmi_status) bind(C, name="get_var_shape")
   !DEC$ ATTRIBUTES DLLEXPORT :: get_var_shape
      integer(kind=c_int)                  :: bmi_status
      character (kind=c_char), intent(in)  :: c_var_name(*)
      integer(c_int), intent(inout)        :: c_var_shape(*)
      character(len=MAXSTRLEN) :: var_name
 
      var_name = char_array_to_string(c_var_name, strlen(c_var_name))
      select case(var_name)
      case ("hgwmod")              ! Heads (m)
          c_var_shape(1) = svat_nr_max
      case ("lvgw","lvgwmodf")     ! Heads inside MetaSWAP (m)
          c_var_shape(1) = svat_nr_max
      case ("phead")               ! pressure heads for MetaSWAP boxes(m)
          c_var_shape(1) = ubound(phead_array, dim=2)
          c_var_shape(2) = ubound(phead_array, dim=1)
      case ("qmodf")               !
          c_var_shape(1) = svat_nr_max 
      case ("nbox")                !
          c_var_shape(1) = svat_nr_max 
      case ("soilselect")          !
          c_var_shape(1) = NMAXSPU
      case ("vsim")                ! ModSimUnsaturatedZoneFlux (m3)
          c_var_shape(1) = svat_nr_max 
      case ("qrun")                ! Excess runoff (m3)
          c_var_shape(1) = svat_nr_max 
      case ("qrot")                ! crop absorbtion (m3) sink term
          c_var_shape(1) = svat_nr_max 
      case ("evsoil")              ! Evap from soil
          c_var_shape(1) = svat_nr_max 
      case ("evpond")              ! Evap from ponding
          c_var_shape(1) = svat_nr_max 
      case ("sc1sim")              ! StorageFactor (m3/m)
          c_var_shape(1) = svat_nr_max
      case ("dtgw","tiop")         ! Surface water timestep and absolute time
          c_var_shape(1) = 1
      case default
          bmi_status = BMI_FAILURE
          return
      end select
      bmi_status = BMI_SUCCESS
   end function get_var_shape
 
 ! -------------------------------------------------------------------------------------------------------------------------------------------
 
   integer(c_int) pure function strlen(char_array)
     character(c_char), intent(in) :: char_array(1000)
     integer :: inull, i
     strlen = 0
     do i = 1, size(char_array)
       if (char_array(i) .eq. C_NULL_CHAR) then
           strlen = i-1
           exit
       end if
     end do
   end function strlen
 
   function char_array_to_string(char_array, length)
     integer(c_int), intent(in) :: length
     character(c_char),intent(in) :: char_array(length)
     character(len=length) :: char_array_to_string
     integer :: i
     do i = 1, length
       char_array_to_string(i:i) = char_array(i)
     enddo
   end function char_array_to_string
 
   function string_to_char_array(string, length)
    integer(c_int),intent(in) :: length
    character(len=length), intent(in) :: string
    character(kind=c_char,len=1) :: string_to_char_array(length+1)
    integer :: i
    do i = 1, length
       string_to_char_array(i) = string(i:i)
    enddo
    string_to_char_array(length+1) = C_NULL_CHAR
   end function string_to_char_array

end module sssbmi
