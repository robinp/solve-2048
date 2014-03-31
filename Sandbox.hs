module Sandbox where

import Main

t1 = Table $
  [[p 1, p 2],
   [e,   p 1]]

gen = decide . head . nexts . snd
