module ReadWrite   
  ( 
  -- * Read
  readMolV2000
  , readPDB
  -- * Write
  , writeXYZ
  , writePDB
  ) where

import Control.Lens
import Control.Monad.State (execState)
import System.Directory (renameFile)
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Data.List (elem)
import Text.Printf (hPrintf)
import Types

-- * Read
-- | Read molecule in *.mol format (V2000)
readMolV2000 :: FilePath -> (Molecule, [Bond])
readMolV2000 inf =
  let txt = (lines . unsafePerformIO . readFile) inf
      counts_line = words (txt !! 3)
      count_atoms = read (counts_line !! 0)
      count_bonds = read (counts_line !! 1)
      atoms_lines = (take count_atoms . drop 4) txt
      bonds_lines = (take count_bonds . drop (4 + count_atoms)) txt
   in (foldr addAtom molecule atoms_lines, foldr addBond [] bonds_lines)
  where
    addAtom a = over atoms ((:) (readline' a))
    addBond b = (:) (readline'' b)
    readline' l =
      execState
        (do let w = words l
            acoordin . x .= read (w !! 0)
            acoordin . y .= read (w !! 1)
            acoordin . z .= read (w !! 2)
            aelement .= w !! 3
            avdwrad .= vdwr (w !! 3))
        atom
    readline'' l =
      execState
        (do let w = words l
            bfid .= read (w !! 0) - 1
            bsid .= read (w !! 1) - 1
            btype .= read (w !! 2)
            bster .= read (w !! 3)
            btop .= read (w !! 4))
        bond

-- | Read molecule in *.pdb format (V2000).
readPDB :: FilePath -> Molecule
readPDB inf =
  let txt = (lines . unsafePerformIO . readFile) inf
      alines = filter (\l -> rfld strip 1 6 l `elem` ["ATOM", "HETATM"]) txt
  in foldr addAtom molecule alines
  where
    strip = filter (/= ' ')
    rfld f s e = f . drop (s - 1) . take e
    addAtom l = over atoms ((:) (parsline l))
    parsline l =
      execState
        (do adatatype .= rfld strip 1 6 l
            aserial .= rfld read 7 11 l
            aname .= rfld strip 13 16 l
            aaltloc .= rfld strip 17 17 l
            aresname .= rfld strip 18 21 l
            achainid .= rfld strip 22 22 l
            aresseq .= rfld read 23 26 l
            arescode .= rfld strip 27 27 l
            acoordin . x .= rfld read 31 38 l
            acoordin . y .= rfld read 39 46 l
            acoordin . z .= rfld read 47 54 l
            aoccup .= rfld read 55 60 l
            atempfac .= rfld read 61 66 l
            aelement .= rfld strip 77 78 l
            avdwrad .= vdwr (rfld strip 77 78 l)
            acharge .= rfld strip 79 80 l)
        atom

-- | Get VDW radius.
vdwr :: Element -> Double
vdwr a =
  case a of
    "H" -> 1.000 -- 1.00 -- 0.500 -- 0.5 -- 1.000
    "O" -> 1.300 -- 1.70 -- 0.650 -- 0.5 -- 1.300
    "N" -> 1.400 -- 1.85 -- 0.700 -- 0.5 -- 1.400
    "C" -> 1.500 -- 1.95 -- 0.750 -- 0.5 -- 1.500
    "S" -> 1.900 -- 2.00 -- 0.5 -- 1.900
    "Br"-> 1.900 -- 2.10 -- 0.950
    "" -> 0.000
    othrewise -> error $ "vdwr not found for: " ++ show a

-- * Write
-- | Write molecule to *.xyz.
writeXYZ :: FilePath -> String -> Molecule -> IO ()
writeXYZ ouf comment molecule = do
  (tmp_name, tmp_handle) <- openTempFile "." "temp"
  hPrint tmp_handle (views atoms length molecule)
  hPutStrLn tmp_handle comment
  mapM_ (writeData tmp_handle) (view atoms molecule)
  hClose tmp_handle
  renameFile tmp_name ouf
  where
    writeData hdl atom = do
      let e = view aelement atom
          (Point x y z) = view acoordin atom
      hPrintf hdl "%s\t%8.6f\t%8.6f\t%8.6f\n" e x y z

-- | Write molecule to *.pdb.
writePDB :: FilePath -> Molecule -> IO ()
writePDB ouf molecule = do
  (tmp_name, tmp_handle) <- openTempFile "." "temp"
  mapM_ (writeAtom tmp_handle) (molecule ^. atoms)
  hClose tmp_handle
  renameFile tmp_name ouf
  where
    writeAtom hdl a =
      let a0 = a ^. adatatype; a1 = a ^. aserial
          a2 = a ^. aname; a3 = a ^. aaltloc ;
          a4 = a ^. aresname; a5 = a ^. achainid; 
          a6 = a ^. aresseq; a7 = a ^. arescode; 
          a8 = a ^. (acoordin . x); a9 = a ^. (acoordin . y); 
          a10 = a ^. (acoordin . z); a11 = a ^. aoccup;
          a12 = a ^. atempfac; a13 = a ^. aelement; 
          a14 = a ^. acharge
          pattern = "%-6s%5d %-4s%1s%-4s%1s%4d%1s   %8.3f%8.3f%8.3f%6.2f%6.2f%12s%2s%2s\n"
      in hPrintf hdl pattern a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 "" a13 a14