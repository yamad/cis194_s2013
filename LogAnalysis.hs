{-# OPTIONS_GHC -Wall #-}
-- | Solution for Homework 2, CIS 194 Spring 2013
-- by Jason Yamada-Hanff, 2016-02-15
module LogAnalysis where

import Log

-- | parse an entire log file
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- | parse one line from a log file
parseMessage :: String -> LogMessage
parseMessage line = makeLM (words line)
  where makeLM ("I":ts:msg)   = LogMessage Info (read ts) (unwords msg)
        makeLM ("W":ts:msg)   = LogMessage Warning (read ts) (unwords msg)
        makeLM ("E":s:ts:msg) = LogMessage (Error $ read s) (read ts) (unwords msg)
        makeLM w              = Unknown (unwords w)


-- | build a message tree
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- | insert a message into a timestamp-sorted message tree
insert :: LogMessage -> MessageTree -> MessageTree
insert lm          Leaf                   =  Node Leaf lm Leaf
insert lm          (Node l (Unknown _) r) = Node l lm r
insert (Unknown _) tree                   = tree
insert lm@(LogMessage _ ts _) (Node l root@(LogMessage _ rTs _) r)
  | ts <= rTs = Node (insert lm l) root r
  | ts >  rTs = Node l root (insert lm r)

-- | return timestamp-ordered list of message
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l root r) = inOrder l ++ [root] ++ inOrder r

-- | report serious errors (at least 50)
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMsg . filter ((>=50) . getSeverity) . msgSort
  where msgSort = inOrder . build
        getMsg (LogMessage _ _ s) = s
        getMsg (Unknown s)        = s
        getSeverity (LogMessage (Error sev) _ _) = sev
        getSeverity _ = 0
