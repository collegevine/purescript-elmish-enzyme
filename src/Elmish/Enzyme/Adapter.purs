module Elmish.Enzyme.Adapter
  ( Adapter
  , unofficialReact_17
  , react_16_4
  , react_16_3
  , react_16_2
  , react_16_1
  , react_15_5
  , react_15_4
  , react_14
  , react_13
  )
  where

foreign import data Adapter :: Type

unofficialReact_17 = adapter "@wojtekmaj/enzyme-adapter-react-17" :: Adapter -- ^17.0.0
react_16_4 = adapter "enzyme-adapter-react-16" :: Adapter                    -- ^16.4.0-0
react_16_3 = adapter "enzyme-adapter-react-16.3" :: Adapter                  -- ~16.3.0-0
react_16_2 = adapter "enzyme-adapter-react-16.2" :: Adapter                  -- ~16.2
react_16_1 = adapter "enzyme-adapter-react-16.1" :: Adapter                  -- ~16.0.0-0 || ~16.1
react_15_5 = adapter "enzyme-adapter-react-15" :: Adapter                    -- ^15.5.0
react_15_4 = adapter "enzyme-adapter-react-15.4" :: Adapter                  --  15.0.0-0 - 15.4.x
react_14 = adapter "enzyme-adapter-react-14" :: Adapter                      -- ^0.14.0
react_13 = adapter "enzyme-adapter-react-13" :: Adapter                      -- ^0.13.0

foreign import adapter :: String -> Adapter
