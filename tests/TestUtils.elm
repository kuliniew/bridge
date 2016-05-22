module TestUtils exposing (generativeTest)

import Check
import Check.Test
import ElmTest


generativeTest : Check.Claim -> ElmTest.Test
generativeTest = Check.Test.evidenceToTest << Check.quickCheck
