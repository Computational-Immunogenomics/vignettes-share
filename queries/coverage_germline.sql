select sampleId, chromosome, position, gene, canonicalEffect, canonicalHgvsCodingImpact, canonicalHgvsProteinImpact, otherTranscriptEffects, spliceRegion
from hmfpatients.germlineVariant
where reported = 1;
