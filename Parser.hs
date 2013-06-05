-- parser produced by Happy Version 1.10

module Parser (parse, WFF(..)) where





import Terms
import Lexer

data HappyAbsSyn 
	= HappyTerminal Token
	| HappyErrorToken Int
	| HappyAbsSyn4 (([WFF], WFF))
	| HappyAbsSyn5 (())
	| HappyAbsSyn6 ([WFF])
	| HappyAbsSyn7 (WFF)
	| HappyAbsSyn8 (Literal)
	| HappyAbsSyn9 ([Term])
	| HappyAbsSyn11 (Term)

type HappyReduction = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> HappyAbsSyn

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47 :: Int -> HappyReduction

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23 :: HappyReduction

action_0 (12) = happyShift action_6
action_0 (13) = happyShift action_7
action_0 (14) = happyShift action_8
action_0 (15) = happyShift action_9
action_0 (17) = happyShift action_10
action_0 (18) = happyShift action_11
action_0 (27) = happyShift action_12
action_0 (28) = happyShift action_13
action_0 (4) = happyGoto action_14
action_0 (6) = happyGoto action_2
action_0 (7) = happyGoto action_3
action_0 (8) = happyGoto action_4
action_0 (11) = happyGoto action_5
action_0 _ = happyReduce_4

action_1 (12) = happyShift action_6
action_1 (13) = happyShift action_7
action_1 (14) = happyShift action_8
action_1 (15) = happyShift action_9
action_1 (17) = happyShift action_10
action_1 (18) = happyShift action_11
action_1 (27) = happyShift action_12
action_1 (28) = happyShift action_13
action_1 (6) = happyGoto action_2
action_1 (7) = happyGoto action_3
action_1 (8) = happyGoto action_4
action_1 (11) = happyGoto action_5
action_1 _ = happyFail

action_2 (16) = happyShift action_26
action_2 _ = happyFail

action_3 (19) = happyShift action_22
action_3 (24) = happyShift action_23
action_3 (25) = happyShift action_24
action_3 (26) = happyShift action_25
action_3 _ = happyFail

action_4 _ = happyReduce_6

action_5 (22) = happyShift action_21
action_5 _ = happyFail

action_6 _ = happyReduce_7

action_7 _ = happyReduce_8

action_8 (18) = happyShift action_20
action_8 _ = happyFail

action_9 (18) = happyShift action_19
action_9 _ = happyFail

action_10 (28) = happyShift action_18
action_10 (9) = happyGoto action_17
action_10 _ = happyReduce_18

action_11 _ = happyReduce_22

action_12 (12) = happyShift action_6
action_12 (13) = happyShift action_7
action_12 (14) = happyShift action_8
action_12 (15) = happyShift action_9
action_12 (17) = happyShift action_10
action_12 (18) = happyShift action_11
action_12 (27) = happyShift action_12
action_12 (28) = happyShift action_13
action_12 (7) = happyGoto action_16
action_12 (8) = happyGoto action_4
action_12 (11) = happyGoto action_5
action_12 _ = happyFail

action_13 (12) = happyShift action_6
action_13 (13) = happyShift action_7
action_13 (14) = happyShift action_8
action_13 (15) = happyShift action_9
action_13 (17) = happyShift action_10
action_13 (18) = happyShift action_11
action_13 (27) = happyShift action_12
action_13 (28) = happyShift action_13
action_13 (7) = happyGoto action_15
action_13 (8) = happyGoto action_4
action_13 (11) = happyGoto action_5
action_13 _ = happyFail

action_14 (30) = happyAccept
action_14 _ = happyFail

action_15 (24) = happyShift action_23
action_15 (25) = happyShift action_24
action_15 (26) = happyShift action_25
action_15 (29) = happyShift action_37
action_15 _ = happyFail

action_16 _ = happyReduce_9

action_17 (22) = happyReduce_23
action_17 _ = happyReduce_16

action_18 (17) = happyShift action_33
action_18 (18) = happyShift action_11
action_18 (11) = happyGoto action_36
action_18 _ = happyFail

action_19 (21) = happyShift action_35
action_19 _ = happyFail

action_20 (21) = happyShift action_34
action_20 _ = happyFail

action_21 (17) = happyShift action_33
action_21 (18) = happyShift action_11
action_21 (11) = happyGoto action_32
action_21 _ = happyFail

action_22 (12) = happyShift action_6
action_22 (13) = happyShift action_7
action_22 (14) = happyShift action_8
action_22 (15) = happyShift action_9
action_22 (17) = happyShift action_10
action_22 (18) = happyShift action_11
action_22 (27) = happyShift action_12
action_22 (28) = happyShift action_13
action_22 (6) = happyGoto action_31
action_22 (7) = happyGoto action_3
action_22 (8) = happyGoto action_4
action_22 (11) = happyGoto action_5
action_22 _ = happyReduce_4

action_23 (12) = happyShift action_6
action_23 (13) = happyShift action_7
action_23 (14) = happyShift action_8
action_23 (15) = happyShift action_9
action_23 (17) = happyShift action_10
action_23 (18) = happyShift action_11
action_23 (27) = happyShift action_12
action_23 (28) = happyShift action_13
action_23 (7) = happyGoto action_30
action_23 (8) = happyGoto action_4
action_23 (11) = happyGoto action_5
action_23 _ = happyFail

action_24 (12) = happyShift action_6
action_24 (13) = happyShift action_7
action_24 (14) = happyShift action_8
action_24 (15) = happyShift action_9
action_24 (17) = happyShift action_10
action_24 (18) = happyShift action_11
action_24 (27) = happyShift action_12
action_24 (28) = happyShift action_13
action_24 (7) = happyGoto action_29
action_24 (8) = happyGoto action_4
action_24 (11) = happyGoto action_5
action_24 _ = happyFail

action_25 (12) = happyShift action_6
action_25 (13) = happyShift action_7
action_25 (14) = happyShift action_8
action_25 (15) = happyShift action_9
action_25 (17) = happyShift action_10
action_25 (18) = happyShift action_11
action_25 (27) = happyShift action_12
action_25 (28) = happyShift action_13
action_25 (7) = happyGoto action_28
action_25 (8) = happyGoto action_4
action_25 (11) = happyGoto action_5
action_25 _ = happyFail

action_26 (12) = happyShift action_6
action_26 (13) = happyShift action_7
action_26 (14) = happyShift action_8
action_26 (15) = happyShift action_9
action_26 (17) = happyShift action_10
action_26 (18) = happyShift action_11
action_26 (27) = happyShift action_12
action_26 (28) = happyShift action_13
action_26 (7) = happyGoto action_27
action_26 (8) = happyGoto action_4
action_26 (11) = happyGoto action_5
action_26 _ = happyFail

action_27 (19) = happyShift action_44
action_27 (24) = happyShift action_23
action_27 (25) = happyShift action_24
action_27 (26) = happyShift action_25
action_27 (5) = happyGoto action_43
action_27 _ = happyReduce_2

action_28 (24) = happyShift action_23
action_28 (25) = happyShift action_24
action_28 (26) = happyShift action_25
action_28 _ = happyReduce_12

action_29 (24) = happyShift action_23
action_29 _ = happyReduce_11

action_30 _ = happyReduce_10

action_31 _ = happyReduce_5

action_32 _ = happyReduce_17

action_33 (28) = happyShift action_18
action_33 (9) = happyGoto action_42
action_33 _ = happyReduce_18

action_34 (12) = happyShift action_6
action_34 (13) = happyShift action_7
action_34 (14) = happyShift action_8
action_34 (15) = happyShift action_9
action_34 (17) = happyShift action_10
action_34 (18) = happyShift action_11
action_34 (27) = happyShift action_12
action_34 (28) = happyShift action_13
action_34 (7) = happyGoto action_41
action_34 (8) = happyGoto action_4
action_34 (11) = happyGoto action_5
action_34 _ = happyFail

action_35 (12) = happyShift action_6
action_35 (13) = happyShift action_7
action_35 (14) = happyShift action_8
action_35 (15) = happyShift action_9
action_35 (17) = happyShift action_10
action_35 (18) = happyShift action_11
action_35 (27) = happyShift action_12
action_35 (28) = happyShift action_13
action_35 (7) = happyGoto action_40
action_35 (8) = happyGoto action_4
action_35 (11) = happyGoto action_5
action_35 _ = happyFail

action_36 (20) = happyShift action_39
action_36 (10) = happyGoto action_38
action_36 _ = happyReduce_20

action_37 _ = happyReduce_15

action_38 (29) = happyShift action_46
action_38 _ = happyFail

action_39 (17) = happyShift action_33
action_39 (18) = happyShift action_11
action_39 (11) = happyGoto action_45
action_39 _ = happyFail

action_40 (24) = happyShift action_23
action_40 (25) = happyShift action_24
action_40 (26) = happyShift action_25
action_40 _ = happyReduce_14

action_41 (24) = happyShift action_23
action_41 (25) = happyShift action_24
action_41 (26) = happyShift action_25
action_41 _ = happyReduce_13

action_42 _ = happyReduce_23

action_43 _ = happyReduce_1

action_44 _ = happyReduce_3

action_45 (20) = happyShift action_39
action_45 (10) = happyGoto action_47
action_45 _ = happyReduce_20

action_46 _ = happyReduce_19

action_47 _ = happyReduce_21

happyReduce_1 = happyReduce 4 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 ((happy_var_1, happy_var_3)
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_0 5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 (()
	)

happyReduce_3 = happySpecReduce_1 5 happyReduction_3
happyReduction_3 _
	 =  HappyAbsSyn5
		 (()
	)

happyReduce_4 = happySpecReduce_0 6 happyReduction_4
happyReduction_4  =  HappyAbsSyn6
		 ([]
	)

happyReduce_5 = happySpecReduce_3 6 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1 7 happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (Lit happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1 7 happyReduction_7
happyReduction_7 _
	 =  HappyAbsSyn7
		 (TRUE
	)

happyReduce_8 = happySpecReduce_1 7 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn7
		 (FALSE
	)

happyReduce_9 = happySpecReduce_2 7 happyReduction_9
happyReduction_9 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (Not happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3 7 happyReduction_10
happyReduction_10 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 :& happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3 7 happyReduction_11
happyReduction_11 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 :| happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3 7 happyReduction_12
happyReduction_12 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 :> happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 4 7 happyReduction_13
happyReduction_13 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (ForAll (mk_var happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 4 7 happyReduction_14
happyReduction_14 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Exists (mk_var happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_3 7 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2 8 happyReduction_16
happyReduction_16 (HappyAbsSyn9  happy_var_2)
	(HappyTerminal (TokenId  happy_var_1))
	 =  HappyAbsSyn8
		 (Pred happy_var_1 happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3 8 happyReduction_17
happyReduction_17 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn8
		 (Pred "Q" [happy_var_1,happy_var_3]
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_0 9 happyReduction_18
happyReduction_18  =  HappyAbsSyn9
		 ([]
	)

happyReduce_19 = happyReduce 4 9 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (happy_var_2 : happy_var_3
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_0 10 happyReduction_20
happyReduction_20  =  HappyAbsSyn9
		 ([]
	)

happyReduce_21 = happySpecReduce_3 10 happyReduction_21
happyReduction_21 (HappyAbsSyn9  happy_var_3)
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2 : happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1 11 happyReduction_22
happyReduction_22 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn11
		 (Var (mk_var happy_var_1)
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2 11 happyReduction_23
happyReduction_23 (HappyAbsSyn9  happy_var_2)
	(HappyTerminal (TokenId  happy_var_1))
	 =  HappyAbsSyn11
		 (Fn happy_var_1 happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 30 30 (error "reading EOF!") (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenKeyword "true" -> cont 12;
	TokenKeyword "false" -> cont 13;
	TokenKeyword "forall" -> cont 14;
	TokenKeyword "exists" -> cont 15;
	TokenKeyword "Conclude" -> cont 16;
	TokenId  happy_dollar_dollar -> cont 17;
	TokenVar happy_dollar_dollar -> cont 18;
	TokenSemi -> cont 19;
	TokenComma -> cont 20;
	TokenDot -> cont 21;
	TokenOp "==" -> cont 22;
	TokenOp "!=" -> cont 23;
	TokenOp "&" -> cont 24;
	TokenOp "|" -> cont 25;
	TokenOp "->" -> cont 26;
	TokenOp "~" -> cont 27;
	TokenBracket '(' -> cont 28;
	TokenBracket ')' -> cont 29;
	}

happyThen = \m k -> k m
happyReturn = \a -> a
happyThen1 = happyThen
happyReturn1 = \a tks -> a

parse_aux tks = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happyError ts
 = error ("Parse error, from tokens: " ++ show (take 5 ts))






parse :: String -> Maybe ([WFF],WFF) 
parse s 
 = case (all_tokens s) of
		Nothing -> Nothing
		Just ts -> Just $ parse_aux ts
{-# LINE 1 "GenericTemplate.hs" -}
{-# LINE 1 "GenericTemplate.hs" -}
-- $Id: GenericTemplate.hs,v 1.11 2001/03/30 14:24:07 simonmar Exp $

{-# LINE 15 "GenericTemplate.hs" -}






















































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

happyAccept j tk st sts (HappyStk ans _) = 

					   (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 127 "GenericTemplate.hs" -}


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = action nt j tk st sts (fn v1 `HappyStk` stk')

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = action nt j tk st sts (fn v1 v2 `HappyStk` stk')

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = action nt j tk st sts (fn v1 v2 v3 `HappyStk` stk')

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk = action nt j tk st1 sts1 (fn stk)
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - (1)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - (1)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError


{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
