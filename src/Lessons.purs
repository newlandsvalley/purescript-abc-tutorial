module Lessons
  ( Lesson
  , lessons
  , last
  , title
  , example
  , hint
  , instruction
  , scoreUrl
  ) where


import Prelude (($), (<>), (-), map)
import Data.Array (index, length)
import Data.Maybe (fromMaybe)

type Lesson =
  { id :: String
  , title :: String
  , instruction :: String
  , example :: String
  , hint :: String
  }

instNotes :: String
instNotes =
   "Use the upper-case characters A-G for the notes of the octave starting from middle C and lower-case a-g for the octave above that." <>
   " In this example, each note has the same length - let's call it the 'unit length' for the moment." <>
   " You can place notes next to each other or separate them with spaces - it won't make much difference to" <>
   " the sound. However, in a score, if they're adjacent then notes with tails will have them joined together."

xmplNotes  :: String
xmplNotes =
  "A B c def"

hintNotes :: String
hintNotes =
  "Try altering some of the notes."

instLongNotesAndBars :: String
instLongNotesAndBars =
  "You can make a note last longer by putting a number after the note name." <>
  " So, for example, c2 represents the note C in the octave immediately above the one that starts with middle C, having a duration of two units." <>
  " Use a vertical bar to introduce a bar line."

xmplLongNotesAndBars :: String
xmplLongNotesAndBars =
  "| c2 cG c2 e2 | g4"

hintLongNotesAndBars :: String
hintLongNotesAndBars =
  "Try experimenting with different note lengths."

instRests :: String
instRests =
  "Use the character z to represent a rest.  In exactly the same manner as for notes, you can set the length of a rest by adding a number after it." <>
  " For example z3 will make the rest last for three units." <>
  " You can spread out the tune into multiple lines if you like by hitting carriage return."

xmplRests :: String
xmplRests =
  "| c2 c2 z cBA |\r\n" <>
  "| E2 A2 z3 A |"

hintRests :: String
hintRests =
  "Try adding another bar which contains both notes and rests."

instOctaves :: String
instOctaves =
  "You can reach the octave below middle C by adding a comma immediately after the note name." <>
  " Each time you add a comma, you drop a further octave. " <>
  " Similarly higher octaves can be reached using apostrophes." <>
  " If you want a longer note, you must put the duration after the comma or apostrophe."

xmplOctaves :: String
xmplOctaves =
  "| C,, G,, C, G, | C G c g | c' g' c''4 |"

hintOctaves :: String
hintOctaves =
  "Experiment by adding some more high or low notes."

instFractionalNotes :: String
instFractionalNotes =
  "You can shorten a note by placing a fraction after the note.  This could be, for example," <>
  " 1/2 or 1/3. A shorthand for 1/2 is simply / and a shorthand for 1/3 is simply /3." <>
  " You can also have longer notes if you use a fraction greater than 1.  Rests are treated the same way." <>
  " If you make notes too short, they may not be heard."

xmplFractionalNotes :: String
xmplFractionalNotes =
  "| C3/2G1/2 E3/2G1/2 C3/2G/ E3/2G/ |"

hintFractionalNotes :: String
hintFractionalNotes =
  "Try experimenting with a succession of notes of different pitch and with different fractional values."

instHornpipe :: String
instHornpipe =
  "The last example was in a hornpipe-like rhythm.  Because this is so common, there is a shorthand for it" <>
  " where you separate each pair of notes with the > character.  This extends the first note by half its length" <>
  " and reduces the second by the same amount."

xmplHornpipe :: String
xmplHornpipe =
  "| C>GE>G C>GE>G |\r\n| c>de>d c>BA>G |"

hintHornpipe :: String
hintHornpipe =
  "If you know it, can you finish off the 'A' part of the tune?"

instStrathspey :: String
instStrathspey =
  "Conversely, you can shorten the first note of a pair and lengthen the second by means of the < character." <>
  " This rhythm is found in strathspeys."

xmplStrathspey :: String
xmplStrathspey =
  "| G | c2 e>c G<c e>g |\r\n| c'2 b>c' a<c' g>e |"

instChords :: String
instChords =
   "You can play a chord by placing a group of notes, adjacent to each other, inside square brackets - for example [acE]." <>
   " To set the duration of the chord, you can either set the length of each note individually or else for the entire chord." <>
   " If you do both, the durations are multiplied together"

xmplChords :: String
xmplChords =
  "| [acE]3 B A2G2 | [eBGE]4 |"

hintChords :: String
hintChords =
  "Try adding another phrase that ends in a chord."

instKeySig :: String
instKeySig =
   "ABC lets you add information that determines how the tune is to be played." <>
   " So far, we have only used the white notes on the piano - i.e. the tune snippets have tended to be in the keys of either" <>
   " C Major or A Minor.  If we want tunes in a different key, we can add what's called a 'K header' where K represents the key signature. " <>
   " A header is usually placed on a line of its own before the melody starts." <>
   " In the key of A, every C, F and G in the melody is implicitly sharpened - this will give a 'major' feel to the chord example."

xmplKeySig :: String
xmplKeySig =
  "K: AMajor \r\n| [acE]3 B A2G2 | [eBGE]4 |"

instFlatKeySig :: String
instFlatKeySig =
  "If your key is a major key, you can, if you want, leave out the word 'Major'.  If it is a flat key, you use 'b' and if a sharp key, '#'. " <>
  " You can also choose to shorten the mode name to just three letters - i.e. BbMaj, BbMajor and Bb are equivalent to each other."

xmplFlatKeySig :: String
xmplFlatKeySig =
   "K: Bb\r\n| BfdB AecA | FdBF D4 |"

instNaturals :: String
instNaturals =
   "If your key means that certain notes are sharpened or flattened, but you need to play the 'natural' " <>
   " (unsharpened or unflattened) note, then you can override the key by using an equals symbol immediately before the note." <>
   " Remember that, as in a score, you only need to mark as natural the first occurrence of the note in any given bar." <>
   " For example, this reintroduces the minor feel although the key is still a major one. Each C in the bar is natural."

xmplNaturals :: String
xmplNaturals =
  "K: AMajor \r\n| A2 B=c dcBc [CEA] |"

instAccidentals :: String
instAccidentals =
   "Similarly, you can sharpen a note by placing a caret symbol (^) immediately before it and flatten it using an underscore" <>
   " symbol (_). If you need a double sharp or double flat, then just double the appropriate symbol." <>
   " This example brings back the major feel although the key is now A Minor. Each C is sharpened."

xmplAccidentals :: String
xmplAccidentals =
 "K: AMinor \r\n| A2 B^c dcBc [CEA] |"

instUnitNote :: String
instUnitNote =
   "You may have noticed when we first introduced notes that we talked about their duration in 'units'.  But how long is a unit?" <>
   " So far, we have used, by default, a convention that it represents an eighth note (a quaver)." <>
   " We can change the unit to be a sixteenth note (a semiquaver) if we use the L (unit note length) header." <>
   " This will have the effect of doubling the speed."

xmplUnitNote :: String
xmplUnitNote =
 "L: 1/16 \r\nA B c def"

instTempo :: String
instTempo =
   "An accurate tempo is defined by means of the Q (tempo) header.  Up till now, we've used a default where we have 120 quarter notes per minute" <>
   " i.e 1/4=120.  We can, for example, slow down our tune firstly by reverting to a unit note length of 1/8 and secondly by explicitly reducing the " <>
   " tempo with the Q header."

xmplTempo :: String
xmplTempo =
  "L: 1/8 \r\nQ: 1/4=60\r\nA B c def"

instMeter :: String
instMeter =
  "The meter is defined with the M header.  For example, a waltz would normally have the meter 3/4 and a march 4/4." <>
  " 3/4 means that each complete bar should have a total duration equal to that of three quarter notes." <>
  " The presence of a meter actually makes little difference to how the tune sounds, but will show up in a score." <>
  " But it is important to make sure that the duration of each complete bar agrees with the meter you designate." <>
  " This example is a slip-jig in 9/8."

xmplMeter :: String
xmplMeter =
  "Q:3/8=120\r\nM:9/8\r\nK:D\r\n" <>
  "ABA A2G F2G | ABA AGF G2E |\r\n" <>
  "ABA A2G F2G | A2d d2c d3 |\r\n" <>
  "A2g f2d e2c | A2B =c2B c2B |\r\n" <>
  "A2g f2d e2^c | A2d d2c d3 |\r\n" <>
  "A2g f2d e2c | A2B =c2B c2^c |\r\n" <>
  "d2A A2G F2G | A2d d2c d3 |\r\n"

instTie :: String
instTie =
  "A tie joins together two notes of the same pitch.  It is indicated by placing a hyphen directly after the first note of the pair." <>
  " The second note may follow immediately, but it can be separated by spaces or even a bar line.  The effect is to play one long note" <>
  " with the combined duration of the pair.  If the notes are of different pitches, the tie will simply be ignored."

xmplTie :: String
xmplTie =
   "| G2 | c2c2 A2Ac | B2B2- B2AB |"

instTriplet :: String
instTriplet =
   "A triplet is usually used if you want to play three notes in the time normally taken by two."  <>
   " You introduce three notes of the same length placed together with the symbol (3" <>
   " This is extremely common in Swedish polskas - for example the start of the Grind Hans Jässpôdspolska."

xmplTriplet :: String
xmplTriplet =
   "K:Dmaj\r\n| A2 | d2 e>f (3g2f2d2 | B4 |"

instComplexTriplet :: String
instComplexTriplet =
   "If your triplet has notes of different lengths, you have to use the complex triplet notation." <>
   " For example (3:2:4d2d2Bd means play the rhythm of three notes in the time of two over the following group" <>
   " of four notes."

xmplComplexTriplet :: String
xmplComplexTriplet =
   "K:Gmaj\r\n| D2 G>A B>c| (3:2:4d2d2Bd g2|"

instQuadruplet :: String
instQuadruplet =
   "Quadruplets are used if you want to play four notes in the time usually taken by three." <>
   " In a similar fashion to triplets, introduce four notes of the same length placed together" <>
   " with the symbol (4. This example contains triplets, a tie and a quadruplet."

xmplQuadruplet :: String
xmplQuadruplet =
   "K:Amaj\r\n| (3efg a2 a>b | (3agf e2-e>e |\r\n| (4f2d2e2c2 | d>f (3f2e2c2 |"

instRepeat :: String
instRepeat =
   "You can indicate that a section should be repeated by sandwiching it between bars which use the colon as a repeat marker - |: and :|" <>
   " The initial repeat marker at the first bar is optional."

xmplRepeat :: String
xmplRepeat =
  "| C2 D2 E2 C2 :|: E2 F2 G4 :|\r\n|: GAGF E2 C2 :|: C2 G,2 C4 :|"

instRepeatVariants :: String
instRepeatVariants =
  "In some tunes, the two repeats may differ in their endings.  You can indicate that using |1 and |2 for the two variant endings"

xmplRepeatVariants :: String
xmplRepeatVariants =
  "L: 1/16\r\nK:Dmaj\r\n|: A4 a4 a2f2 | gfga b3a g2f2 |\r\n| e3f g2b2 a2g2 | f3e d2c2 d2B2 |\r\n" <>
  "|1 B2A^G A8 :|2 B2AG F2EF A2A,2 | A,2D2 D8 |"

instTitle :: String
instTitle =
   "Very many of our previous examples have had no headers - only the melody line.  But, in fact a legitimate ABC tune always" <>
   " requires some headers.  The first is largely irrelevant - a reference number denoted by X.  Any number will do" <>
   " in most cases. The second header must be the tune title - T. You should also include the L (note length) and  M (meter) headers" <>
   " introduced earlier. Finally, the K (key) header should always be the last one."

xmplTitle :: String
xmplTitle =
  "X:1\r\nT:Camptown Races\r\nM:4/4\r\nL:1/8\r\nK:D\r\n|AAFA|BAF2|FE2z|FE2z|AAFA|BAF2|E2FE|D2-D2|\r\n|D>DFA|d4|B>BdB|A3F|\r\nAA F/2F/2 A/2A/2|BAF2|EF/2-G/2FE|D4 |\r\n"

instRhythm :: String
instRhythm =
  "You can use the R (rhythm) header to indicate the type of tune (jig, reel, polska etc.). In most ABC collections, this field is optional." <>
  " However, if you want to save your tune to tradtunedb, it requires a rhythm header to be present so that you can search" <>
  " easily for tunes of the same type"

xmplRhythm :: String
xmplRhythm =
  "X: 1\r\nT: Kapten Lindholms Engelska\r\nR: engelska\r\nM: 4/4\r\nL: 1/8\r\nK:Amaj\r\n" <>
  "|: ed | cAce dcdf | ecAF E2 ed |\r\n| cABc defg | aece agfe | cAce dcdf |\r\n| ecAF E2 ed | cABc defg | a2 ag a2 :|\r\n" <>
  "|: e2 | aac'e aac'e | bbd'e bbd'e | aac'e aac'e |\r\n| efed cB A2| fdfa ecea | fdfa ecea |\r\n| fdfa gegb | baag a2 :|\r\n"

instInformation :: String
instInformation =
  "There are various other headers that you can use to add information about the tune as free text.  The most important are these: " <>
  " C (composer), O (geographical origin), S (source - where or how the tune was collected) and Z (the tune transcriber)."

xmplInformation :: String
xmplInformation =
  "X: 1\r\nT: Gubbdansen\r\nS: from 12 låtar för 2 eller 3 fioler med Gärdebylåten i Hjort Anders Olssons originalsättning\r\n" <>
  "Z: John Batchellor\r\nR: polska\r\nM: 3/4\r\nL: 1/16\r\nK:Dmin\r\n" <>
  "|: f3g f4 a4 | a2ba g2ag f2e2 | d3e f2g2 a2f2 | f3e e2^c2 A4 :|\r\n" <>
  "|: ^c2c2 d2d2 e2e2 | f2f2 gfed e4 | ^c2c2 d2d2 e2e2 | f2f2 gfed e4 |\r\n" <>
  "a4 b2a2 g2f2 | f2ef g2f2 e2d2 | fed^c c4 d4 :|\r\n"

instChangeKey :: String
instChangeKey =
  "If a tune changes key, you can indicate this simply by placing the K (key) header inside the score at the point where the key changes." <>
  " In this example, the first part of the tune is in B Minor and the second part in F# Minor." <>
  " Various other headers can be used within the score in this way - in particular, the M (meter) and L (unit note length) headers."

xmplChangeKey :: String
xmplChangeKey =
  "T:Polska från Småland \r\nM:3/4\r\nL:1/16\r\nR:polska\r\nK:Bmin\r\n" <>
  "|: B4 A4 B4 | d2f2 e2dc c2d2 | B2B2 A2A2 B2B2 |d2f2 e2dc d4 |\r\n" <>
  "F2GA B2AB c2Bc |d2cd edcB A2F2 | F2GA B2AB c2Bc |d2cd edcB A2F2 |\r\n" <>
  "F2GA B2c2 d3B | B2A2 B8 :|\r\n" <>
  "K:F#Min\r\n" <>
  "|: f4 e4 f4 |g2a2 b2ag g2a2 |f2f2 e2e2 f2f2 |g2a2 b2ag a4 |\r\n" <>
  "c2de f2ef g2fg |a2ga bagf e2c2 | c2de f2ef g2fg |a2ga bagf e2c2 |\r\n" <>
  "c2de f2g2 a3f |f2e2 f8 :|\r\n"

instChangeKeyTransient :: String
instChangeKeyTransient =
  "You can also mark a transient key change by placing the K (key) header in the body of the tune score, but enclosed within square brackets."

xmplChangeKeyTransient :: String
xmplChangeKeyTransient =
  "X:1\r\nQ:1/4=80\r\nM:2/4\r\nK:C\r\n| C,E,G,C |[K:A] A,CEA |\r\n|[K:B] B,DFB |[K:C] CEGc |\r\n"

instMixolydian :: String
instMixolydian =
  "If you come across a modal tune, rather than marking its key signature as straightforward major or minor,"  <>
  " you can instead use the mode name. For example, the following tune is in D Mixolydian.  But remember, the classical" <>
  " modes all use the standard diatonic scale - they just start at different places along the scale. So for this tune " <>
  " the printed score would look, to all intents and purposes, identical to that for E Minor. Feel free to use either signature."

xmplMixolydian :: String
xmplMixolydian =
  "X: 1\r\nT: The Yellow Wattle\r\nR: jig\r\nM: 6/8\r\nL: 1/8\r\nK: Dmix\r\n" <>
  "|:dcA AGE|ABA ABc|dcA ABc|dcA AGE|\r\n" <>
  "dcA AGE|ABA AGE|EDD cde|dcA GED:|\r\n" <>
  "|:DED c3|ded c3|DED cde|dcA GED|\r\n" <>
  "DED c3|ded d2c|ABA ABc|dcA GED:|\r\n"

instKlezmer :: String
instKlezmer =
  "Klezmer tends to use modes that are not diatonic scales - some intervals are more than two semitones." <>
  " Suppose you have a tune that would be in a 'standard' mode except that one note in the scale is sharpened." <>
  " You can either use the name of the mode in the key signature and then explicitly sharpen this note each time it occurs in the score" <>
  " or you can modify the key signature itself, adding as many (sharpened or flattened) accidentals as are needed." <>
  " The following tune is in D Dorian, but with every G sharpened."

xmplKlezmer :: String
xmplKlezmer =
  "X: 1\r\nT: Der Badchen Freylach \r\nM: 2/4\r\nL: 1/16\r\nK: Ddor^G\r\n" <>
  "|: DA,DF GAGF | A2A2 FED2 | DA,DF GAGF | A4 A4- |\r\n" <>
  "| AA,DF GAFD | A2A2 FED2 | EFGF EDEF | D8 :|\r\n" <>
  "|: ABcB dcBA | GABc A4 | dcBA GABc | A4 A4 |\r\n" <>
  "| ABcB dcBA | GABc A4 |1 ABcB AB (3FED | EFD2- D4 |\r\n" <>
  ":|2 GABA GAFE | D8 :||\r\n"

instBalkan :: String
instBalkan =
 "Balkan music also tends to have unusual modes and time signatures.  This tune is in A Minor with a sharpened G; the meter is 11/16." <>
 " The '~' symbol indicates a particular decoration - a roll - but this player does not attempt it."

xmplBalkan :: String
xmplBalkan =
  "X: 1\r\nT: Acano mlada nevesto\r\nO: Macedonia\r\nS: R.B.Iverson\r\nM: 11/16\r\nL: 1/16\r\nK: AMin^G\r\n" <>
  "|: E3  e2e2 d2c2 | ~B2A ~A2GA B2-B2 :: A2A dccB BAAG |\r\n" <>
  "| G2F ~F2EF ~G2FE | A2A dccB BAAG | ~G2F ~FGFE E2E2 :|\r\n" <>
  "|: EFD EFGA BcBA | Bcd cBcA BEeE |\r\n" <>
  "|  EFD EFGA BcBA | GAB AGFE E2-E2 :|\r\n"


lessons :: Array Lesson
lessons =
  [
    { id : "notes", title : "the notes", instruction : instNotes, example : xmplNotes, hint : hintNotes }
  , { id : "longnotesandbars", title : "long notes and bars", instruction : instLongNotesAndBars, example : xmplLongNotesAndBars, hint : hintLongNotesAndBars }
  , { id : "rests", title : "rests", instruction : instRests, example : xmplRests, hint : hintRests }
  , { id : "octaves", title : "octaves", instruction : instOctaves, example : xmplOctaves, hint : hintOctaves  }
  , { id : "fractionalnotes", title : "fractional notes", instruction : instFractionalNotes, example : xmplFractionalNotes, hint : hintFractionalNotes }
  , { id : "hornpipes", title : "hornpipes", instruction : instHornpipe, example : xmplHornpipe, hint : hintHornpipe }
  , { id : "strathspeys", title : "strathspeys", instruction : instStrathspey, example : xmplStrathspey, hint : "" }
  , { id : "chords", title : "chords", instruction : instChords, example : xmplChords, hint : hintChords }
  , { id : "keysignature", title : "key signature", instruction : instKeySig, example : xmplKeySig, hint : "" }
  , { id : "sharpandflatkeys", title : "sharp and flat key signatures", instruction : instFlatKeySig, example : xmplFlatKeySig, hint : "" }
  , { id : "naturals", title : "naturals", instruction : instNaturals, example : xmplNaturals, hint : "" }
  , { id : "accidentals", title : "sharps and flats", instruction : instAccidentals, example : xmplAccidentals, hint : "" }
  , { id : "unitnote", title : "how long is a unit note?", instruction : instUnitNote, example : xmplUnitNote, hint : "" }
  , { id : "tempo", title : "tempo", instruction : instTempo, example : xmplTempo, hint : "" }
  , { id : "meter", title : "meter", instruction : instMeter, example : xmplMeter, hint : "" }
  , { id : "tie", title : "tie", instruction : instTie, example : xmplTie, hint : "" }
  , { id : "triplet", title : "triplet", instruction : instTriplet, example : xmplTriplet, hint : "" }
  , { id : "complextriplet", title : "triplet with differing note lengths", instruction : instComplexTriplet, example : xmplComplexTriplet, hint : "" }
  , { id : "quadruplet", title : "quadruplet", instruction : instQuadruplet, example : xmplQuadruplet, hint : "" }
  , { id : "repeats", title : "repeats", instruction : instRepeat, example : xmplRepeat, hint : "" }
  , { id : "repeatvariants", title : "repeats with variant endings", instruction : instRepeatVariants, example : xmplRepeatVariants, hint : "" }
  , { id : "title", title : "tune title", instruction : instTitle, example : xmplTitle, hint : "" }
  , { id : "rhythm", title : "rhythm", instruction : instRhythm, example : xmplRhythm, hint : "" }
  , { id : "information", title : "information headers", instruction : instInformation, example : xmplInformation, hint : "" }
  , { id : "keychanges", title : "key changes", instruction : instChangeKey, example : xmplChangeKey, hint : "" }
  , { id : "keychangetransient", title : "transient key changes", instruction : instChangeKeyTransient, example : xmplChangeKeyTransient, hint : "" }
  , { id : "modes", title : "other modes", instruction : instMixolydian, example : xmplMixolydian, hint : "" }
  , { id : "klezmer", title : "klezmer", instruction : instKlezmer, example : xmplKlezmer, hint : "" }
  , { id : "balkan", title : "Balkan", instruction : instBalkan, example : xmplBalkan, hint : "" }
  ]

last :: Int
last =
  length lessons - 1

fetch :: Int -> (Lesson -> String) -> String
fetch i f =
  fromMaybe "unexpected error" $ map f $ index lessons i

title :: Int -> String
title i =
  fetch i _.title

example :: Int -> String
example i =
  fetch i _.example

hint :: Int -> String
hint i =
  fetch i _.hint

instruction :: Int -> String
instruction i =
  fetch i _.instruction

scoreUrl :: Int -> String
scoreUrl i =
  let
    score = fetch i _.id
  in
    "assets/images/tutorial/" <> score <>  ".png"
