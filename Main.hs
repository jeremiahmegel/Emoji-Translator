{-
	Copyright Jeremiah Megel 2015

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Main (main) where

import Data.Char (chr, isAlpha, isHexDigit, toLower)
import Data.Function (on)
import Data.List (concat, dropWhileEnd, intersect, intersperse, sortBy)
import Data.List.Split (splitOn)
import Data.List.Unique (count)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Numeric (readHex)
import System.IO (getContents)
import Text.XML.Light (Element(..), QName(..), cdData, elContent, findAttr, findChild, onlyElems, onlyText, parseXMLDoc)

type Emoji = String
type Annotation = String
type Synonyms = [Set.Set String]
type Score = Double

getAssociations :: String -> IO [([Annotation], [Emoji])]
getAssociations filename = do
	contents <- readFile filename
	return $
		map
			(\ x ->
				(
					splitOn "; " $ cdData $ head $ onlyText $ elContent x
					,
					readEmoji $
						init $ tail $ fromMaybe "" $
							findAttr (QName "cp" Nothing Nothing) x
				)
			)
			$
			onlyElems $ elContent $
				fromMaybe (Element (QName "" Nothing Nothing) [] [] Nothing) $
					findChild (QName "annotations" Nothing Nothing) $
						fromMaybe (Element (QName "" Nothing Nothing) [] [] Nothing) $
							parseXMLDoc $ contents
	where
		readEmoji [] =
			[]
		readEmoji ('{' : cs) =
			let
				(multiChar, _ : es) =
					break (== '}') cs
				in
				multiChar : readEmoji es
		readEmoji (c1 : ('-' : (c2 : cs))) =
			map
				(: [])
				[c1 .. c2] ++ (readEmoji cs)
		readEmoji (c : cs) =
			[c] : readEmoji cs

getNames :: String -> IO (Map.Map Char String)
getNames filename = do
	contents <- readFile filename
	return $
		foldr
			(\ l acc ->
				Map.insert
					(chr $ fst $ head $ readHex $ takeWhile isHexDigit l)
					(tail $ tail $ dropWhile (/= '#') l)
					acc
			)
			Map.empty
			$
			filter
				(\ l ->
					(not . null) l
					&&
					(isHexDigit . head) l
				)
				$
				lines contents

getThesaurus :: String -> IO (Map.Map String [String])
getThesaurus filename = do
	contents <- readFile filename
	return $
		foldr
			(\ (w : ss) acc ->
				Map.insert w ss acc
			)
			Map.empty
			$
			map (splitOn ",") $
				splitOn "\r" $ map toLower contents

getFreq :: String -> IO (Map.Map String Double)
getFreq filename = do
	contents <- readFile filename
	return $
		let
			(m, (most, least)) =
				(
					foldr
						(\ l (m, (most, least)) ->
							let
								(w, _ : n) =
									span (/= '\t') l
								in
								(
									Map.insert w (read n) m
									,
									(
										max most $ read n,
										min least $ read n
									)
								)
						)
						(
							Map.empty
							,
							(0, maxBound :: Int)
						)
						$
						lines contents
				)
			in
			Map.map
				(\ x ->
					1 / (fromIntegral $
						2 ^ (
							(floor $ logBase 2 $
								fromIntegral most / fromIntegral least
							)
							-
							(floor $ logBase 2 $
								fromIntegral most / fromIntegral x
							)
							+ 1
						)
					)
				)
				m

getInflections :: String -> IO (Map.Map String (Set.Set String))
getInflections filename = do
	contents <- readFile filename
	return $
		foldr
			(\ l m ->
				let
					entry =
						(
							(\ (x, y) ->
								(
									extractWord $ head $ words x
									,
									filter (not . null) $
										map extractWord $
											filter ((/= '{') . head) $
												words $ tail y
								)
							) $
								span (/= ':') l
						)
					in
					foldr
						(\ k ->
							(Map.insertWith
								Set.union
								(fst entry)
								(Set.singleton k)
							)
							.
							(Map.insertWith
								Set.union
								k
								(Set.union
									(Set.delete k $ Set.fromList $ snd entry)
									$
									Set.singleton $ fst entry
								)
							)
						)
						m
						$ snd entry
			)
			Map.empty
			$
			lines contents

extractWord :: String -> String
extractWord =
	(map toLower)
	.
	(dropWhileEnd $ not . isAlpha)
	.
	(dropWhile $ not . isAlpha)

allSynonyms :: Map.Map String [String] -> Map.Map String (Set.Set String) -> String -> Synonyms
allSynonyms thes inf s =
	map fst $
		iterate
			(\ (prev, banned) ->
				let
					newBanned =
						Set.union prev banned
				in
				(
					Set.difference
						(Set.fold
							(\ w syns ->
								foldr
									Set.insert
									syns
									$
									fromMaybe
										[]
										$
										Map.lookup w thes
							)
							Set.empty
							prev
						)
						newBanned
					,
					newBanned
				)
			)
			(
				Set.union
					(Set.singleton s)
					$
					fromMaybe
						Set.empty
						$
						Map.lookup s inf
				,
				Set.empty
			)

connectScore :: Int -> Synonyms -> Synonyms -> Score
connectScore depth s1 s2 =
	foldr
		(\ n1 score ->
			foldr
				(\ n2 ->
					(
						+
						(((/) `on` fromIntegral)
							(Set.size $ Set.intersection (s1 !! n1) (s2 !! n2))
							$
							2 ^ (n1 + n2)
						)
					)
				)
				score
				[0 .. depth]
		)
		0
		[0 .. depth]

connect :: Map.Map String Double -> [([Synonyms], [Emoji])] -> [(Synonyms, Int)] -> [([(Annotation, Score)], [Emoji])]
connect freq assoc text =
	foldr
		(\ (as, es) -> (
			(
				foldr
					(\ a -> (
						(
							head $ Set.toList $ head a
							,
							foldr
								(\ (w, n) -> (
									+
									(
										(fromMaybe 1 $
											Map.lookup
												(head $ Set.toList $ head w)
												freq
										)
										*
										connectScore 1 w a
										*
										fromIntegral n
									)
								))
								0
								text
						)
						:
					))
					[]
					as
				,
				es
			)
			:
		))
		[]
		assoc

scoreEmoji :: [([(Annotation, Score)], [Emoji])] -> Map.Map Emoji Score
scoreEmoji =
	foldr
		(\ (as, es) emojiScores ->
			foldr
				(\ e ->
					Map.insertWith (+) e $
						foldr ((+) . snd) 0 as
				)
				emojiScores
				es
		)
		Map.empty

analyze :: Map.Map String [String] -> [([Annotation], [Emoji])] -> Map.Map String Double -> Map.Map String (Set.Set String) -> String -> [(Emoji, Score)]
analyze thes assoc freq inf =
	sortBy (compare `on` snd)
	.
	Map.toList
	.
	scoreEmoji
	.
	connect
		freq
		(map
			(\ (as, es) ->
				(
					map (allSynonyms thes inf) as
					,
					es
				)
			)
			assoc
		)
	.
	map
		(\ (w, n) ->
			(allSynonyms thes inf w, n)
		)
	.
	count
	.
	(map extractWord)
	.
	words

output :: Map.Map Char String -> [(Emoji, Score)] -> String
output n =
	foldr
		(\ (e, s) -> (
			(
				e ++ " ("
				++
				(
					concat $ intersperse ", " $
						map
							(\ c ->
								fromMaybe "" $ Map.lookup c n
							)
							e
				)
				++
				")" ++ " = " ++ (show s) ++ "\n"
			)
			++
		))
		""
	.
	filter ((> 0) . snd)

main = do
	t <- getThesaurus "mobythes.aur"
	a <- getAssociations "en.xml"
	n <- getNames "allkeys_CLDR.txt"
	f <- getFreq "count_1w.txt"
	i <- getInflections "2of12id.txt"
	text <- getContents
	putStrLn $ output n $ analyze t a f i text

