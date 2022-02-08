val output :
  to_directory:string ->
  title:string ->
  tab_size:int ->
  source_paths:string list ->
  ignore_missing_files:bool ->
  cov_from:string ->
  cov_to:string ->
  ?cov_from_label:string ->
  ?cov_to_label:string ->
  theme:[ `Light | `Dark | `Auto ] ->
  unit ->
  unit
