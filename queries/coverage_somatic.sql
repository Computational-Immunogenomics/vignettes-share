select sampleId, chromosome, position, gene, canonicalEffect, canonicalHgvsCodingImpact, canonicalHgvsProteinImpact, otherTranscriptEffects, spliceRegion
from hmfpatients.somaticVariant
where reported = 1;
