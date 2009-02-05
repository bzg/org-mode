source("SweaveSyntaxOrg.R")
source("RweaveOrg.R")

Sweave("testing.Rorg", driver=RweaveOrg, syntax=SweaveSyntaxOrg)
Stangle("testing.Rorg", driver=Rtangle(), syntax=SweaveSyntaxOrg)
