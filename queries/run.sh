o_dir=/data/tmp/vignettes-dev/util/data/query/

execute_sql_on_prod coverage_cn.sql > ${o_dir}coverage_cn.txt
execute_sql_on_prod coverage_germline.sql > ${o_dir}coverage_germline.txt  
execute_sql_on_prod coverage_somatic.sql > ${o_dir}coverage_somatic.txt
execute_sql_on_prod cn_amp_del.sql > ${o_dir}cn_amp_del.txt     
execute_sql_on_prod gie_somatic.cql > ${o_dir}gie_somatic.txt   
execute_sql_on_prod gie_cn.sql > ${o_dir}gie_cn.txt  
execute_sql_on_prod gie_hr.sql > ${o_dir}gie_hr.txt  
execute_sql_on_prod gie_somatic.cql > ${o_dir}gie_somatic.txt  
execute_sql_on_prod somatic_variant.sql > ${o_dir}somatic_variant.txt
