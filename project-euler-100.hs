import System.IO

--   B       B - 1     1
-- ----- * --------- = -
-- B + R   B + R - 1   2
--
-- (B + R) (B + R - 1)
-- ------------------- = 2
--      B (B - 1)
--
-- B² + 2 BR + R² - B - R = 2 B² - B
--
-- 2 BR + R² - R = B²
--
-- B² - 2 BR - R² - R = 0
--
-- (B - R)² = R
--
-- Therefore B - R is sqrt(R).
--
-- And B + R = sqrt(R) + 2 R.
--
-- Thus, we choose R such that sqrt(R) + 2 R is bigger than 10^12.
--
-- B + R is the smallest possible above 10^12.

main = do print 0
