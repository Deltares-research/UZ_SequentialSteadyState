!  ExampleSSS.f90 
!
!  FUNCTIONS:
!  ExampleSSS - Instructive application of sequential steady state class
!

!****************************************************************************
!
!  PROGRAM: ExampleSSS
!
!  PURPOSE:  Illustrates the use of the sequential steady state class simulating the unsaturated zone
!
!****************************************************************************

program ExampleSSS_coupled
use globals
use sss
use dumpres
use database_io
implicit none
type (t_SequentialSteadyStateSingle) :: svat(2)
type (t_sssparam) :: param
type(t_databaseSet), target :: dbset
character(len=:), allocatable :: dbpath
real(kind=hp), allocatable :: theta(:), h(:) 
real(kind=hp) :: pond, qtop, qbot, qrot, gwl, reva, dtgw, sc1
logical :: suc6
integer :: itime, iter
integer, parameter :: ntime = 200
integer, parameter :: niter = 1 ! 200
integer, parameter :: nbox = 18
real(kind=hp) :: qrain(ntime), qevap(ntime)
real(kind=hp) :: gwl_val

! results to netcdf file for check
type (t_dumpnc) :: dump
type (t_results) :: results

qrain(1:60) = 0.0045
qrain(61:80) = 0.0026
qrain(81:100) =0.0032
qrain(101:ntime) =0.d0
qrain(:) = 0.d0
qevap(:) = 0.02

dtgw = 1.d0

! Read set of databases. Each database corresponds with a soil physical unit and a rootzone depth
! dbpath contains all the netcdf files, dbset is the instance holding the resulting set
dbpath = "D:/leander/MetaSWAP/test_compare_megaswap/van_hendrik_new/metaswap_a/unsa_nc"
suc6 = dbset%readNCset(dbpath)

! Fill convenient struct with parameters for this sequential steady state instance
param%area         = 100.d0   ! area
param%top          =   0.d0   ! elevation
param%spu          = 300      ! soil physical unit number
param%init_phead   =  -1.5136 ! initial phead
param%init_gwl     =  -3.d0   ! initial gwl
param%dprz         =   1.d0   ! rootzone depth, thickness
param%zmax_ponding =   0.02   ! ponding reservoir depth
param%maxinf       =   0.0032 ! infiltration rate limit
param%soil_resist  =   1.d0   ! soil resistance

suc6 = svat(1)%initialize(param,dbset)   ! init svat sequential steady state instance
allocate(h(nbox))                     ! vertical profile of pressure heads
allocate(theta(nbox))                 ! vertical profile of moisture content

! open some files for output
call results%init(nbox)
call dump%init('results.nc') 

! set timestep
dtgw = 1.d0                           ! timestep size
h = param%init_phead                  ! initialize the heads profile with a constant value
gwl_val = -200.
do itime = 1, ntime                   ! time-loop
   gwl_val = gwl_val + 1.d0
   call svat(1)%prepare(dtgw,qrain(itime),qevap(itime))
   do iter=1, niter                   ! iteration-loop
       results%gwl = gwl_val
       call svat(1)%calc(results%gwl, &
                      results%h,      &
                      results%th,     &
                      results%qbot,   &
                      results%sc1)   ! set initial groundwater level as the current level, no modflow interaction

       ! storage coefficients to mf6
       ! recharge flux to mf6
       ! mf6_solve
   enddo ! iter

   call svat(1)%finalize(qrot,       &
                      results%gwl,   &
                      results%pond,  &
                      results%qrun,  &
                      results%qmodf, &
                      results%reva)
   results%qrain=qrain(itime)
   results%peva=qevap(itime)

   call dump%dump(itime*1.d0, results)
   write(0,'(a,i4.4,a)') 'Timestep #', itime, ' completed  ... '
enddo ! itime
call dump%close() 
end program ExampleSSS_coupled

