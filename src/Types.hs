{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Control.Lens

type Serial = Int
type Resseq = Int
type Resname = String
type Element = String

data Point = Point
    { _x :: Double
    , _y :: Double
    , _z :: Double
    }
  deriving (Show)

data Atom = Atom
    { _adatatype :: String -- ^ "ATOM"/"HETATM"
    , _aserial :: Serial -- ^ Atom serial number
    , _aname :: String -- ^ Atom name
    , _aaltloc :: String -- ^ Alternate location indicator. 
    , _aresname :: String -- ^ Residue name
    , _achainid :: String -- ^ Chain identifier
    , _aresseq :: Int -- ^ Residue sequence number
    , _arescode :: String -- ^ Code for insertions of residues
    , _acoordin :: Point -- ^ (X,Y,Z) orthogonal Å coordinate
    , _aoccup :: Double -- ^ Occupancy
    , _atempfac :: Double -- ^ Temperature factor
    , _aelement :: Element -- ^ Element symbol
    , _avdwrad :: Double -- ^ Van der Waals radii
    , _acharge :: String -- ^ Atom charge
    }
  deriving (Show)

data Bond = Bond
    { _bfid :: Serial
    , _bsid :: Serial
    , _btype :: Int
    , _bster :: Int
    , _btop :: Int
    }
  deriving (Show)

newtype Molecule = Molecule 
  { _atoms :: [Atom]
  }
  deriving (Show)

molecule = Molecule 
  { _atoms = []
  }

point = Point
  { _x = 0
  , _y = 0
  , _z = 0
  }

atom = Atom 
  { _adatatype = ""
  , _aserial = 0
  , _aname = ""
  , _aaltloc = ""
  , _aresname = ""
  , _achainid = ""
  , _aresseq = 0
  , _arescode = ""
  , _acoordin = point
  , _aoccup = 0
  , _atempfac = 0
  , _aelement = ""
  , _avdwrad = 0
  , _acharge = ""
  }

bond = Bond 
  { _bfid = 0
  , _bsid = 0 
  , _btype = 0
  , _bster = 0 
  , _btop = 0
  }

makeLenses ''Point
makeLenses ''Atom
makeLenses ''Molecule
makeLenses ''Bond
