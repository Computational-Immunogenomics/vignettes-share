o_dir=/data/tmp/vignettes/util/data/query/

execute_sql_on_prod 0_somatic_reported.sql > ${o_dir}panel_somatic_reported.txt; echo "somatic done!" 
execute_sql_on_prod 1_germline_reported.sql > ${o_dir}panel_germline_reported.txt; echo "germline done!"
execute_sql_on_prod 2_copy_number.sql > ${o_dir}panel_copy_number.txt; echo "copy number done!"
