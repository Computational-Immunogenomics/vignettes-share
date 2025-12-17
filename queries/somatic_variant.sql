select sampleId, type, worstCodingEffect, avg(subclonalLikelihood <= .5) as clonal, count(1) as ct 
from hmfpatients.somaticVariant 
where filter = "PASS" 
group by sampleId, type, worstCodingEffect;