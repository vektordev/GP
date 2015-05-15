module GRPAgentBinding
( reprogram
, act
, reinforcement
)
where

import GRPStats
import GRPCommon

--When one of these three is called for the first time, eval initial state and inject. That is, when compilation is needed.

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
