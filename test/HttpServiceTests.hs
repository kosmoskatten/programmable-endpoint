{-# LANGUAGE OverloadedStrings #-}
module HttpServiceTests
       ( suite
       ) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import Simulation.Node.Service.Http

suite :: Test.Framework.Test
suite = testGroup "HttpService tests"
        [ testCase "Service specific selfStore paths shall be returned back"
                   selfStoreShallReturnServiceSpecificPath
        ]

-- | Test that a selfStore specific for the service is returned
-- back. The selfStore path should be like 'httpServices/<service
-- name>'
selfStoreShallReturnServiceSpecificPath :: Assertion
selfStoreShallReturnServiceSpecificPath = do
  let snaps = map snd $
        toSnapRoutes
        [ Routes [ ("url1", selfStore)
                 , ("url2", selfStore) ] `as` "service1"
        , Routes [ ("url3", selfStore)
                 , ("url4", selfStore) ] `as` "service2"
        ]
  results <- mapM (evalHandler $ setRequestType GetRequest) snaps
  assertEqual "Shall be equal"
              [ "httpServices/service1/"
              , "httpServices/service1/"
              , "httpServices/service2/"
              , "httpServices/service2/" ]
              results

