{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.X11.Xlib.VisualInfo
-- Copyright   : (c) 2012 Anthony DeRossi
-- License     : BSD-style (see the file LICENSE)
--
-- Maintainer  : Anthony DeRossi <ajderossi@gmail.com>
-- Stability   : provisional
-- Portability : portable
--
-- A collection of FFI definitions for obtaining information about X11
-- visuals from Xlib
--
-----------------------------------------------------------------------------

module Graphics.X11.Xlib.VisualInfo (
  -- * Visual information type
    VisualInfo(..)
  , emptyVisualInfo

  -- * Visual information masks
  , VisualInfoMask
  , visualNoMask
  , visualIDMask
  , visualScreenMask
  , visualDepthMask
  , visualClassMask
  , visualRedMaskMask
  , visualGreenMaskMask
  , visualBlueMaskMask
  , visualColormapSizeMask
  , visualBitsPerRGBMask
  , visualAllMask

  -- * Visual information functions
  , getVisualInfo
  , matchVisualInfo
  ) where

import Foreign

import Graphics.X11.Types
import Graphics.X11.Xlib.Types

#include <X11/Xutil.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct { char x; t (y); }, y)

foreign import ccall unsafe "XFree" xFree :: Ptr a -> IO ()

-- | Information about an X11 visual (see @man 3 XVisualInfo@)
data VisualInfo = VisualInfo
  { visualInfo_visual :: Visual
  , visualInfo_visualID :: VisualID
  , visualInfo_screen :: ScreenNumber
  , visualInfo_depth :: #type int
  , visualInfo_class :: #type int
  , visualInfo_redMask :: #type unsigned long
  , visualInfo_greenMask :: #type unsigned long
  , visualInfo_blueMask :: #type unsigned long
  , visualInfo_colormapSize :: #type int
  , visualInfo_bitsPerRGB :: #type int
  }
  deriving (Show)

-- | An empty 'VisualInfo' structure
emptyVisualInfo :: VisualInfo
emptyVisualInfo = VisualInfo
  { visualInfo_visual = Visual nullPtr
  , visualInfo_visualID = 0
  , visualInfo_screen = 0
  , visualInfo_depth = 0
  , visualInfo_class = 0
  , visualInfo_redMask = 0
  , visualInfo_greenMask = 0
  , visualInfo_blueMask = 0
  , visualInfo_colormapSize = 0
  , visualInfo_bitsPerRGB = 0
  }

instance Storable VisualInfo where
  sizeOf _ = #size XVisualInfo
  alignment _ = #alignment XVisualInfo
  peek p = do
    visual <- Visual `fmap` #{peek XVisualInfo, visual} p
    visualID <- #{peek XVisualInfo, visualid} p
    screen <- #{peek XVisualInfo, screen} p
    depth <- #{peek XVisualInfo, depth} p
    class_ <- #{peek XVisualInfo, class} p
    redMask <- #{peek XVisualInfo, red_mask} p
    greenMask <- #{peek XVisualInfo, green_mask} p
    blueMask <- #{peek XVisualInfo, blue_mask} p
    colormapSize <- #{peek XVisualInfo, colormap_size} p
    bitsPerRGB <- #{peek XVisualInfo, bits_per_rgb} p
    return $ VisualInfo
               { visualInfo_visual = visual
               , visualInfo_visualID = visualID
               , visualInfo_screen = screen
               , visualInfo_depth = depth
               , visualInfo_class = class_
               , visualInfo_redMask = redMask
               , visualInfo_greenMask = greenMask
               , visualInfo_blueMask = blueMask
               , visualInfo_colormapSize = colormapSize
               , visualInfo_bitsPerRGB = bitsPerRGB
               }
  poke p info = do
    #{poke XVisualInfo, visual} p visualPtr
    #{poke XVisualInfo, visualid} p $ visualInfo_visualID info
    #{poke XVisualInfo, screen} p $ visualInfo_screen info
    #{poke XVisualInfo, depth} p $ visualInfo_depth info
    #{poke XVisualInfo, class} p $ visualInfo_class info
    #{poke XVisualInfo, red_mask} p $ visualInfo_redMask info
    #{poke XVisualInfo, green_mask} p $ visualInfo_greenMask info
    #{poke XVisualInfo, blue_mask} p $ visualInfo_blueMask info
    #{poke XVisualInfo, colormap_size} p $ visualInfo_colormapSize info
    #{poke XVisualInfo, bits_per_rgb} p $ visualInfo_bitsPerRGB info
    where
      ~(Visual visualPtr) = visualInfo_visual info

-- | Visual information mask bits
type VisualInfoMask = #{type long}
#{enum VisualInfoMask,
  , visualNoMask = VisualNoMask
  , visualIDMask = VisualIDMask
  , visualScreenMask = VisualScreenMask
  , visualDepthMask = VisualDepthMask
  , visualClassMask = VisualClassMask
  , visualRedMaskMask = VisualRedMaskMask
  , visualGreenMaskMask = VisualGreenMaskMask
  , visualBlueMaskMask = VisualBlueMaskMask
  , visualColormapSizeMask = VisualColormapSizeMask
  , visualBitsPerRGBMask = VisualBitsPerRGBMask
  , visualAllMask = VisualAllMask
  }

-- | An interface to the Xlib function @XGetVisualInfo()@
getVisualInfo :: Display -> VisualInfoMask -> VisualInfo -> IO [VisualInfo]
getVisualInfo dpy mask template
  = alloca $ \nItemsPtr ->
    with template $ \templatePtr -> do
      itemsPtr <- xGetVisualInfo dpy mask templatePtr nItemsPtr
      if itemsPtr == nullPtr
        then return []
        else do
          nItems <- peek nItemsPtr
          items <- peekArray (fromIntegral nItems) itemsPtr
          _ <- xFree itemsPtr
          return items

foreign import ccall unsafe "XGetVisualInfo" xGetVisualInfo
  :: Display -> VisualInfoMask -> Ptr VisualInfo -> Ptr #{type int}
  -> IO (Ptr VisualInfo)

-- | An interface to the Xlib function @XMatchVisualInfo()@
matchVisualInfo :: Display -> ScreenNumber -> #{type int} -> #{type int}
  -> IO (Maybe VisualInfo)
matchVisualInfo dpy screen depth class_
  = alloca $ \infoPtr -> do
    status <- xMatchVisualInfo dpy screen depth class_ infoPtr
    if status == 0
      then return Nothing
      else do
        info <- peek infoPtr
        return $ Just info

foreign import ccall unsafe "XMatchVisualInfo" xMatchVisualInfo
  :: Display -> ScreenNumber -> #{type int} -> #{type int} -> Ptr VisualInfo
  -> IO #{type Status}
