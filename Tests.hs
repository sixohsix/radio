
import HUnit

import Radio.Util (joinPath)

tests = test [
  "joinPath" ~: do
     _ <- assertEqual "joins okay if dir has trailing slash" "a/b" (joinPath "a/" "b")
     _ <- assertEqual "joins okay if dir has no trailing slash" "a/b" (joinPath "a" "b")
  ]
