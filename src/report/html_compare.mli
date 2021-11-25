val output :
  to_directory:string ->
  title:string ->
  tab_size:int ->
  theme:[ `Light | `Dark | `Auto ] ->
  source_paths:string list ->
  ignore_missing_files:bool ->
  cov_x:string ->
  cov_y:string ->
  unit
