module GRPAgentBinding
( reprogram
, act
, reinforcement
)
where

import GRPStats
import GRPCommon

--This file is currently DEAD CODE. Compilation is done by calling ghc and execution is done by calling the generated executables.
--Documentation of that procedure is in GRPHeadless

--The best bet to get this up and running probably is plugin.

--When one of these three is called for the first time, eval initial state and inject. That is, when compilation is needed.
--Afterwards, store State to AgentStats

reprogram :: AgentStats -> (AgentStats, String)
reprogram ag = (ag, "")
act :: AgentStats -> Input -> (AgentStats, Output)
act ag inp = (ag, [])
reinforcement :: AgentStats -> Int -> String -> AgentStats
reinforcement ag _ _ = ag

--In Agent:
--		reprogram :: [StdGen] -> State -> [String] -> (String, State)
--		act :: [StdGen] -> State -> Input -> (Output, State)
--		reinforcement :: [StdGen] -> State -> Int -> String -> State



--TODO: What about safety assertions? What about compiler feedback and fitness eval? Who takes care of stripping source code of it's prefix and reattaching it?

--create Object file. Load it. Evaluate Initial state. All as required.
--compile :: AgentStats -> AgentStats

--Unload a Agent's object file.
--unload
