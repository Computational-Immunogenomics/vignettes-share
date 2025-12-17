select a.sampleId, a.gene, event, germline, evidenceType, a.level, onLabel, direction, isCanonical, minCopyNumber, maxCopyNumber
from hmfpatients.protect a
left join hmfpatients.geneCopyNumber b on (a.sampleId = b.sampleId and a.gene = b.gene)
where a.reported = 1 
and a.evidenceType not like '%EXPRESSION%'
and a.event like binary '%gain%' or a.event like binary '%loss%'
and a.direction like '%RESPONSIVE%'
and a.level != "D";
