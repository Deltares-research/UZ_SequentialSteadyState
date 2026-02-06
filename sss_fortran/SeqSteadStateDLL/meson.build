bmi_sources = files(
    'SeqSteadState_bmi.f90',
    '..' / 'Prototype' / 'bmi' / 'bmi.f90', 
    '..' / 'Prototype' / 'prototype_comp.f90',
    '..' / 'Prototype' / 'database_io.f90',
    '..' / 'Prototype' / 'meta_tb.f90',
    '..' / 'Prototype' / 'protoglobal.f90',
    '..' / 'Prototype' / 'sss.f90',
    '..' / 'Prototype' / 'StorageFormulation.f90',
    '..' / 'Prototype' / 'UnsaturatedZone.f90',
    )

library('seqsteadstate', bmi_sources,
         link_with: [netcdf],
         name_prefix: 'lib', 
         dependencies: [netcdff],
         install: true)
