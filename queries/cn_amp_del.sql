WITH requested_samples AS (
  SELECT sampleId FROM hmfpatients.datarequest_all
)

SELECT 
  a.sampleId, 
  b.start, 
  b.end, 
  b.gene, 
  c.primaryTumorLocation, 
  b.maxCopyNumber, 
  a.ploidy,
  (b.maxCopyNumber > a.ploidy * 3) AS high_amp,
  (b.maxCopyNumber < 0.5) AS homozygous_del
FROM requested_samples rs
JOIN hmfpatients.purity a ON rs.sampleId = a.sampleId
JOIN hmfpatients.geneCopyNumber b ON a.sampleId = b.sampleId
LEFT JOIN hmfpatients.clinical c ON a.sampleId = c.sampleId
WHERE 
  b.maxCopyNumber > a.ploidy * 3 OR 
  b.maxCopyNumber < 0.5;
