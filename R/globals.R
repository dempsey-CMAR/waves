


utils::globalVariables(c(
  # wv_read_txt
  "year",
  "yr",
  "mo",
  "da",
  "hr",
  "mn",
  "sc",

  # write_report_table
  "Station_Name",
  "Depl_Date", "Depl_Lat", "Depl_Lon", "Recv_Date",
  "Inst_Model", "Inst_Serial", "Inst_Depth", "Inst_Altitude",
  "Bin_Size", "First_Bin_Range", "Notes", "CMAR_ID",
  "Waves_Averaging_Interval_s",
  "Waves_Ensemble_Interval_s",
  "Waves_PingsPerEnsemble",
  "Depl_Duration",
  "Depl_Sounding",
  "Depth Sounding (m)",

  # format_report_table
  "Record",

  # helpers
  "depl_date",
  "Hs",
  "H1/10",
  "H1/3",
  "Hmax",
  "Tp",
  "T1/10",
  "T1/3",
  "Tmax",
  "Dp",
  "Depth",
  "CM",
  "CD",

  #  wv_plot_ts
  "variable",
  "variable_title",
  "value",
  "timestamp_utc",
  "x",

  # wv_test_grossrange
  "height_m",
  "period_s",
  "grossrange_flag",
  "gr_min",
  "gr_max",
  "user_min",
  "user_max",
  "height_flag",
  "period_flag",

  "wv_thresholds",

  # adjust_variable_names
  "column_names",
  ".",

  #wv_test_rate_of_change
  "depth_diff",
  "depth_trim_flag",
  #"rate_of_change_flag",
  #"rate_of_change_flag_sensor_depth_below_surface_m",
  "sensor_depth_below_surface_m",

  # wv_plot_depth_flags
  "depth_flag",

  # wv_summarise_flags
  "flag_value",
  "grossrange_flag_value",
  "n_fl",
  "n_obs",
  "qc_test"

))
