''A LOVE LETTER TO FREEBASIC :: FREEBASIC EVOLUTION :: V1.1
''written in FREEBASIC by TOURIST TRAP in April 2019
''with the help of the community

'#define   _DEBUG
#macro _P(id)
   ? #id, id :
#endMacro
#include "fbgfx.bi"
#include once "inc/dodicat_fonts.bas"
? "dodicat's fonts inside..."

dim shared as fb.IMAGE ptr    titlepic, biohazardpic, hangarPic, quitPic
dim shared as fb.IMAGE ptr    biopic, biolitpic, pilotpic, labcrewpic, biodarkpic, fxm, dkl, cod, cpi, stw
dim shared as fb.IMAGE ptr    lockedpanpic, modulepic
dim shared as any ptr         starter, starter2, starter3, starter4

'Credit::FBCommunity
#ifDef __FB_WIN32__
#print "compiling to .. a win32 API platform"
   ? "running Landeel's DPI awareness system..."
   #include once "inc/landeelDpiAwarerenessSetter.bas"
   #undef _alpha   
	declare function TimeBeginPeriod alias "timeBeginPeriod" (as ulong=1) as long
	declare function TimeEndPeriod alias "timeEndPeriod" (as ulong=1) as long
#else
#print "compiling to a win32 API platform"
	#define TimeBeginPeriod
	#define TimeEndPeriod
#endIf
#ifnDef _SLEEP1MS
   #macro _SLEEP1MS()
      TimeBeginPeriod
      sleep 1
      TimeEndPeriod
   #endMacro
#else
#print __FILE__ , _SLEEP1MS already defined
#endIf

namespace CreditFBDoc
   Function BMPlOAD( ByRef filename As Const String ) As Any Ptr
	    Dim As Long filenum, bmpwidth, bmpheight
	    Dim As Any Ptr img
	    '' open BMP file
	    filenum = FreeFile()
	    If Open( filename For Binary Access Read As #filenum )<>0 Then Return 0
	        '' retrieve BMP dimensions
	        Get #filenum, 19, bmpwidth
	        Get #filenum, 23, bmpheight
	    Close #filenum
	    '' create image with BMP dimensions
	    img = ImageCreate( bmpwidth, Abs(bmpheight) )
	    If img = 0 Then Return 0
	    '' load BMP file into image buffer
	    If BLoad( filename, img )<>0 Then ImageDestroy( img ): Return 0
	    Return img
	End Function
end namespace

'-------------------------------------------------DECL--------------------------
#define  _void
#ifnDef _pi
#define _pi					   ( 4*atn(1) )
#else
#print __FILE__ , _pi already defined
#endIf

#ifnDef _MIN
#define _MIN(a, b)			( iif((a)<(b), (a), (b)) )
#else
#print __FILE__ , _MIN already defined
#endIf

#ifnDef _MAX
#define _MAX(a, b)			( iif((a)>(b), (a), (b)) )
#else
#print __FILE__ , _MAX already defined
#endIf

''note:  there may be redundancy in this program
#ifDef __FB_64BIT__
   extern p alias "__fb_gfx" as integer ptr
#else
   extern p alias "__fb_gfx" as long ptr
#endIf
nameSpace djRedundo
   'author: DJ.Peters@fb.net
   function IsScreenLocked() as boolean
      if screenPtr()=0 then return FALSE
      #ifndef __FB_64BIT__
        #define index 33
      #else
        #define index 44
      #endif
      return iif(p[index] and 1,true,false)
      #undef index
   end function
   
   #include "inc/fbsound_dynamic.bi"
   function FbSound_Init() as boolean
      dim as string  data_path
      data_path = exepath() & "/dta/"
      dim as boolean ok
      '
      ok=fbs_Init()
      if ok=false then
         ? "error: fbs_Init() !"
         ? FBS_Get_PlugError()
         beep
         sleep
      end if
      return ok
   end function
   function LoadWave(byref HWave as integer, byref WaveFileName as const string) as boolean
      dim as string  data_path
      data_path = exepath() & "/dta/"
      dim as boolean ok
      '
      ok=fbs_Load_WAVFile(data_path & WaveFileName,@HWave)
      if ok=false then
         ? "error: fbs_Load_WAVFile() !"
         beep
         sleep
      end if
      'get next free playback channel or create one
      'ok=fbs_Play_Wave(HWave)
      'if ok=false then
      '  ? "error: fbs_Play_Wave() !"
      '  beep
      '  sleep
      'end if
      return ok
   end function
end nameSpace
? "DJ.Peters' FbSound inside..."
? "DJP sound init =", djRedundo.FbSound_Init()
dim as integer testWave
dim as integer testWav2
dim as integer testWav3
dim as integer testWav4
? "add sound fbsloop44.wav =", djRedundo.LoadWave(testWave, "fbsloop44.wav")
? "add sound atem.wav =", djRedundo.LoadWave(testWav2, "atem.wav")
? "add sound pcar.wav =", djRedundo.LoadWave(testWav3, "pcar.wav")
? "add sound rnb_loop.wav =", djRedundo.LoadWave(testWav4, "rnb_loop.wav")
'djRedundo.fbs_Play_Wave(testWave,1)
'djRedundo.fbs_Play_Wave(testWav2,1)
'djRedundo.fbs_Play_Wave(testWav3,1)
'djRedundo.fbs_Play_Wave(testWav4,1)

nameSpace ttRedundo
   #macro _ExitforOnEscapeKeyPressed
      if inkey=chr(27) then exit for
   #endMacro

   type SCREENTEST extends OBJECT
      'global variable container
      'storing screen parameter
      declare static sub TestScreen()
      static as integer  scrW
      static as integer  scrH
   end type
   dim as integer SCREENTEST.scrW       => -1
   dim as integer SCREENTEST.scrH       => -1
   sub SCREENTEST.TestScreen()
      screenInfo (SCREENTEST.scrW, SCREENTEST.scrH)
   end sub

   type INTERACTIONTEST extends SCREENTEST
      'global variable container
      'storing mouse/keyboard interaction
      declare static function TestMouse() as long
      declare static function TestKeyboard() as long
      static as long  gmX
      static as long  gmY
      static as long  gmBtn
      static as long  scanCode
   end type
   dim as long INTERACTIONTEST.gmX        => -1
   dim as long INTERACTIONTEST.gmY        => -1
   dim as long INTERACTIONTEST.gmBtn      => -1
   dim as long INTERACTIONTEST.scanCode   => -1
   function INTERACTIONTEST.TestMouse() as long
      return getMouse ( INTERACTIONTEST.gmX, _
                        INTERACTIONTEST.gmY, _
                        , _
                        INTERACTIONTEST.gmBtn   )
   end function
   function INTERACTIONTEST.TestKeyboard() as long
      dim as long scanCodeResult => -1
      if multiKey(fb.SC_BACKSPACE) then
         scanCodeResult = fb.SC_BACKSPACE
      elseif   multiKey(fb.SC_SPACE)   then
         scanCodeResult = fb.SC_SPACE
      elseif   multiKey(fb.SC_LEFT)    andAlso   multiKey(fb.SC_UP)      then
         scanCodeResult = fb.SC_LEFT + fb.SC_UP
      elseif   multiKey(fb.SC_LEFT)    andAlso   multiKey(fb.SC_DOWN)    then
         scanCodeResult = fb.SC_LEFT + fb.SC_DOWN
      elseif   multiKey(fb.SC_RIGHT)   andAlso   multiKey(fb.SC_UP)      then
         scanCodeResult = fb.SC_RIGHT + fb.SC_UP
      elseif   multiKey(fb.SC_RIGHT)   andAlso   multiKey(fb.SC_DOWN)    then
         scanCodeResult = fb.SC_RIGHT + fb.SC_DOWN
      elseif   multiKey(fb.SC_LEFT)    then
         scanCodeResult   =    fb.SC_LEFT
      elseif   multiKey(fb.SC_RIGHT)   then
         scanCodeResult = fb.SC_RIGHT
      elseif   multiKey(fb.SC_DOWN)    then
         scanCodeResult = fb.SC_DOWN
      elseif   multiKey(fb.SC_UP)      then
         scanCodeResult = fb.SC_UP
      end if
      '
      while inkey<>"" : /'clean keyboard buffer'/ :wend
      INTERACTIONTEST.scanCode = scanCodeResult
      '
      return scanCodeResult
   end function
end nameSpace

#macro _COMPLEMENTCOLOR(value, depth)
   value xor rgba(200,100,100,140) 
#endMacro
#macro _DYNARRAYINCREASE(array)
   redim preserve array(uBound(array) + 1)
#endMacro
#macro _DYNARRAYDECREASE(array)
   if uBound(array)>0 then
      redim preserve array(uBound(array) - 1)
   else
      erase array
   end if
#endMacro
#macro _DYNARRAYINCREASEBYRANGE(array, range)
   redim preserve array(uBound(array) + range)
#endMacro
#macro _ARRAYLASTITEM(array)
   array(uBound(array))
#endMacro
#macro _IMGCREATE(imgvariablename, imgW, imgH, bckgColor, imgColorDepth)
   imgvariablename = imageCreate(imgW, imgH, bckgColor, imgColorDepth)
#endMacro
#macro _IMGRESET(imgvariablename, bckgColor, imgColorDepth)
   scope
      dim as integer   imgW, imgH
      imageInfo imgvariablename, imgW, imgH
      imageDestroy(imgvariablename)
      imgvariablename = imageCreate(imgW, imgH, bckgColor, imgColorDepth)
   end scope
#endMacro
#macro _IMGDESTROY(imgvariablename)
   imageDestroy(imgvariablename)
   imgvariablename   = 0
#endMacro
nameSpace ttDrgDrpBtn
   const as double   _2pi => 8*atn(1)

   type BIVALUE
         as integer   _bvx
         as integer   _bvy
   end type

   type POSITIONXY
         as integer   _x
         as integer   _y
   end type
   declare operator -(byval as POSITIONXY, byval as POSITIONXY) as POSITIONXY
   declare operator <>(byval as POSITIONXY, byval as POSITIONXY) as boolean
   operator -(byval LeftOperand as POSITIONXY, byval RightOperand as POSITIONXY) as POSITIONXY
      dim as POSITIONXY   operationResult
      operationResult._x = LeftOperand._x - RightOperand._x
      operationResult._y = LeftOperand._y - RightOperand._y
      '---->
      return operationResult
   end operator
   operator <>(byval LeftOperand as POSITIONXY, byval RightOperand as POSITIONXY) as boolean
      if LeftOperand._x<>RightOperand._x   orElse _ 
         LeftOperand._y<>RightOperand._y   then
         '---->
         return TRUE
      else
         '---->
         return FALSE
      end if
   end operator

   type WIDHEI
      #define _refreshErrorType   typeOf(Err())
      declare operator Cast() as integer
      declare property Maximum() as integer
      declare property RatioForEllipse() as single
      declare function Xf(byval as integer) as integer
      declare function Yf(byval as integer) as integer
      declare function Refresh(  byval as integer=-1, _ 
                                 byval as integer=-1) _ 
                                 as _refreshErrorType
         as integer   _wid
         as integer   _hei
         as integer   _maximum
         as single   _ratioForEllipse
      #undef _refreshErrorType
   end type
   operator WIDHEI.Cast() as integer
      '---->
      return THIS._wid
   end operator
   property WIDHEI.Maximum() as integer
      '---->
      return -_MIN(-THIS._wid, -THIS._hei)
   end property
   property WIDHEI.RatioForEllipse() as single
      '---->
      return THIS._hei/THIS._wid
   end property
   function WIDHEI.Xf(byval Xi as integer) as integer
      '---->
      return Xi + THIS._wid - 1
   end function
   function WIDHEI.Yf(byval Yi as integer) as integer
      '---->
      return Yi + THIS._hei - 1
   end function

   type DROPZONE
      enum _DROPZONESHAPE
         _fullRectangle
         _maximumEllipse
      end enum
      declare constructor()
      declare property Xi() as integer
      declare property Yi() as integer
      declare property Xf() as integer
      declare property Yf() as integer
      declare function IsInDropzoneShape overload( byval as integer, _
                                                   byval as integer  ) _ 
                                                   as boolean
      declare function IsInDropzoneShape(byval as POSITIONXY) as boolean
      declare sub DrawDropZone()
         as _DROPZONESHAPE  _dzShape
         as POSITIONXY      _topLeftCorner
         as WIDHEI          _widHei
         as string          _keyId
         '
      declare sub DrawDropzoneOccupability()
         as integer        _capacity
         as integer        _occupiedSlotCount
         as integer        _arrayOfDroppedPtr(any)
   end type
   type DZ   as DROPZONE
   constructor DROPZONE()
      dim as integer   scrW, scrH
         screenInfo   scrW, scrH
      '
      with THIS
         ._dzShape            => DZ._DROPZONESHAPE._fullRectangle
         ._topLeftCorner._x   => scrW\4
         ._topLeftCorner._y   => scrH\3
         ._widHei._wid        => scrW\8
         ._widHei._hei        => scrH\8
         ._keyId              => "ID"& str(int(rnd*9e+12))
         ._capacity           => 2
         ._occupiedSlotCount  => 0
      end with
   end constructor
   property DZ.Xi() as integer
      '---->
      return THIS._topLeftCorner._x
   end property
   property DZ.Yi() as integer
      '---->
      return THIS._topLeftCorner._y
   end property
   property DZ.Xf() as integer
      '---->
      return THIS._widHei.Xf(THIS._topLeftCorner._x)
   end property
   property DZ.Yf() as integer
      '---->
      return THIS._widHei.Yf(THIS._topLeftCorner._y)
   end property
   function DZ.IsInDropzoneShape(   byval PosX as integer, _
                                    byval PosY as integer   ) _ 
                                    as boolean
       select case THIS._dzShape
          case DZ._DROPZONESHAPE._fullRectangle
             if PosX>=THIS.Xi   andAlso _
                PosX<=THIS.Xf   andAlso _
                PosY>=THIS.Yi   andAlso _ 
                PosY<=THIS.Yf   then
                '---->
                return TRUE
             else
                '---->
                return FALSE
             end if
          case else
             'DZ._DROPZONESHAPE._maximumEllipse
             if PosX>=THIS.Xi   andAlso _
                PosX<=THIS.Xf   andAlso _
                PosY>=THIS.Yi   andAlso _ 
                PosY<=THIS.Yf   then
                dim as single   xCenter => (THIS.Xi + THIS.Xf)\2
                dim as single   yCenter => (THIS.Yi + THIS.Yf)\2
                dim as single   rX => THIS._widhei._wid\2
                dim as single   rY => THIS._widhei._hei\2
               if (((PosX - xCenter)/rX)^2 + ((PosY - yCenter)/rY)^2)<1 then
                  '---->
                  return TRUE
               else
                  '---->
                  return FALSE
               end if
             else
                '---->
                return FALSE
             end if
       end select
   end function
   function DZ.IsInDropzoneShape(byval PosXY as POSITIONXY) as boolean
       select case THIS._dzShape
          case DZ._DROPZONESHAPE._fullRectangle
             if PosXY._x>=THIS.Xi   andAlso _
                PosXY._x<=THIS.Xf   andAlso _
                PosXY._y>=THIS.Yi   andAlso _ 
                PosXY._y<=THIS.Yf   then
                '---->
                return TRUE
             else
                '---->
                return FALSE
             end if
          case else
             'DZ._DROPZONESHAPE._maximumEllipse
             if PosXY._x>=THIS.Xi   andAlso _
                PosXY._x<=THIS.Xf   andAlso _
                PosXY._y>=THIS.Yi   andAlso _ 
                PosXY._y<=THIS.Yf   then
                dim as single   xCenter => (THIS.Xi + THIS.Xf)\2
                dim as single   yCenter => (THIS.Yi + THIS.Yf)\2
                dim as single   rX => THIS._widhei._wid\2
                dim as single   rY => THIS._widhei._hei\2
               if (((PosXY._x - xCenter)/rX)^2 + ((PosXY._y - yCenter)/rY)^2)<1 then
                  '---->
                  return TRUE
               else
                  '---->
                  return FALSE
               end if
            else
                '---->
                return FALSE
             end if
       end select
   end function
   sub DZ.DrawDropZone()
      static as integer   counter
      static as boolean   isFlipped
      '
      'get background color
      dim as integer bkgColor
      screenControl fb.GET_COLOR, , bkgColor
      'get screen color depth
      dim as integer   scrDepth
      screenInfo   , , scrDepth
      '
      line (THIS.Xi, THIS.Yi)-(THIS.Xf, THIS.Yf), _ 
            (_COMPLEMENTCOLOR(bkgColor, scrDepth)) * 30000, _ 
            b, _ 
            iif(isFlipped, &b1111000011111111, &b0000111111110000)
      select case THIS._dzShape
         case DZ._DROPZONESHAPE._fullRectangle
            line (THIS.Xi + 1, THIS.Yi + 1)-(THIS.Xf - 1, THIS.Yf - 1), _ 
                  _COMPLEMENTCOLOR(bkgColor, scrDepth), _ 
                  b, _ 
                  iif(isFlipped, &b1111000011111111, &b0000111111110000)
         case else
               'DZ._DROPZONESHAPE._maximumEllipse
               circle ((THIS.Xi + THIS.Xf)\2, (THIS.Yi + THIS.Yf)\2), _
                        THIS._widhei.Maximum\2, _ 
                        RGBA(100,100,100,100) , _ 
                        (_pi/180)*(counter + 10), _ 
                        (_pi/180)*(counter + 20), _ 
                        THIS._widhei.RatioForEllipse
             circle ((THIS.Xi + THIS.Xf)\2, (THIS.Yi + THIS.Yf)\2), _
                        THIS._widhei.Maximum\2, _ 
                        RGBA(100,100,100,100) , _ 
                        (_pi/180)*(counter + 30), _ 
                        (_pi/180)*(counter + 40), _ 
                        THIS._widhei.RatioForEllipse
       end select
      '
      counter += 1
      'if counter>310 then counter = 0
      if (counter mod 12)=0 then isFlipped = not isFlipped
      '
      THIS.DrawDropzoneOccupability()
      '
   end sub
   sub DZ.DrawDropzoneOccupability()
      for i as integer = 1 to THIS._capacity
         circle ((THIS.Xi + THIS.Xf)\2 + i*4, (THIS.Yi + THIS.Yf)\2), 2, rgb(0,200,200)  
      next i
      for i as integer = 1 to THIS._occupiedSlotCount
         circle ((THIS.Xi + THIS.Xf)\2, (THIS.Yi + THIS.Yf)\2 + 6 + i*4), 2, rgb(200,0,0)
      next i
   end sub

   type DRAGDROPBUTTON
      enum _DGDPBTNSHAPE
         _naked
         _squared
         _rounded
      end enum
      declare constructor()
      declare property FontWidth() as integer
      declare property FontHeight() as integer
      declare property Xi() as integer
      declare property Yi() as integer
      declare property Xf() as integer
      declare property Yf() as integer
      declare property RoundedShapeRadius() as BIVALUE
      declare property DropZoneCount() as integer
      declare sub AddDropZoneToArrayOfPtr(byval as DROPZONE ptr)
      declare sub AddDropZoneArrayOfPtrToArrayOfPtr(() as DROPZONE ptr)
      declare sub RemoveAllDropZoneFromArrayOfPtr()
      declare sub RemoveDropZoneFromArrayOfPtrAtIndexAtIndex(byval as integer)
      declare sub RemoveDropZoneFromArrayOfPtrAtIndexByKeyId(byval as string)
      declare sub TestScreen()
      declare function IsInButtonShape(byval as integer, _ 
                                       byval as integer) _ 
                                       as boolean
      declare sub TestMouse()
      declare sub ShowAllDropZone()
      declare sub ShowDropZoneAtIndex(byval as integer)
      declare sub ShowDropZoneAtKeyId(byval as string)
      declare sub DrawDgdpbtn()
         as _DGDPBTNSHAPE    _btnShape
         as string          _btnTxt
         as POSITIONXY      _topLeftCorner
         as WIDHEI          _widHei
         '
         as integer          _dpZoneCount
         as DROPZONE ptr     _dpZoneArrayPtr(any)
         as fb.IMAGE ptr     _dgdpbtnImage
         as ulong            _idleColour
         as ulong            _mouseOverColour
         as ulong            _mouseClickColour
         as ulong            _mouseDragColour
         as ulong            _mouseDroppableColour
         as ulong            _mouseDroppedColour
         as boolean          _hasMouseOver
         as boolean          _hasMouseClick
         as POSITIONXY       _offsetAtClick
         as Boolean          _hasMultiplicityIndicator
         as Boolean          _isDragAllowed
         as boolean          _hasDragStarted
         as boolean          _isMouseOverDropZone
         as boolean          _isMouseDropped
         as integer          _dropZoneCandidateindex
         '
         as integer          _scrW
         as integer          _scrH
         as integer          _scrDepth
         as integer          _colWidth
         as integer          _rowWidth
         as integer          _roundedShapeRadius
      static as integer               constructionCount
      static as integer               arrayOfDragDropButtonPtrCount
      static as DRAGDROPBUTTON ptr    arrayOfDragDropButtonPtr(any)
   end type
   type DGDPBTN as DRAGDROPBUTTON
   dim as integer         DGDPBTN.constructionCount
   dim as integer         DGDPBTN.arrayOfDragDropButtonPtrCount
   dim as DGDPBTN ptr     DGDPBTN.arrayOfDragDropButtonPtr(any)
   constructor DRAGDROPBUTTON()
      DGDPBTN.constructionCount += 1
      _DYNARRAYINCREASE(DGDPBTN.arrayOfDragDropButtonPtr)
      _ARRAYLASTITEM(DGDPBTN.arrayOfDragDropButtonPtr) => @THIS
      '
      THIS.TestScreen()
      '
      with THIS
         ._btnShape              => DGDPBTN._DGDPBTNSHAPE._naked
         ._btnTxt                => "OK"
         ._topLeftCorner         => type<POSITIONXY>(THIS._scrW/2, THIS._scrH/2)
         ._widHei._wid           => _MAX(18, .FontWidth*len(._btnTxt))
         ._widHei._hei           => .FontHeight + 4
         '
         .AddDropZoneToArrayOfPtr(0)
         _IMGCREATE(._dgdpbtnImage, ._widHei._wid, ._widHei._hei, RGBA(100,100,100,100), RGBA(200,100,100,100))
         ' 
         ._idleColour            => RGBA(100,100,100,100)
         ._mouseOverColour       => RGBA(100,200,200,100)
         ._mouseClickColour      => RGBA(100,100,200,100)
         ._mouseDragColour       => RGBA(200,100,100,100)
         ._mouseDroppableColour  => RGBA(100,200,100,200)
         ._mouseDroppedColour    => RGBA(200,200,100,100)
         ._isDragAllowed         => TRUE
         ._hasMouseOver          => FALSE   
         ._hasMouseClick         => FALSE
         ._offsetAtClick         => type<POSITIONXY>(-1, -1)
         ._hasDragStarted        => FALSE
         ._isMouseOverDropZone   => FALSE
         ._isMouseDropped        => FALSE
         ._dropZoneCandidateindex   => -1
      end with
   end constructor
   property DGDPBTN.FontWidth() as integer
      '---->
      return 16 'loWord(cLng(width()))
   end property
   property DGDPBTN.FontHeight() as integer
      '---->
      return 16 'hiWord(width())
   end property
   property DGDPBTN.Xi() as integer
      '---->
      return THIS._topLeftCorner._x
   end property
   property DGDPBTN.Yi() as integer
      '---->
      return THIS._topLeftCorner._y
   end property
   property DGDPBTN.Xf() as integer
      '---->
      return THIS._widHei.Xf(THIS._topLeftCorner._x)
   end property
   property DGDPBTN.Yf() as integer
      '---->
      return THIS._widHei.Yf(THIS._topLeftCorner._y)
   end property
   property DGDPBTN.RoundedShapeRadius() as BIVALUE
      dim as BIVALUE   returnValue
      returnValue._bvx = THIS._widHei._wid\6
      returnValue._bvy = THIS._widHei._hei\2
      '---->
      return returnValue
   end property
   property DGDPBTN.DropZoneCount() as integer
      '---->
      return THIS._dpZoneCount
   end property
   sub DGDPBTN.AddDropZoneToArrayOfPtr(byval DZitem as DROPZONE ptr)
      if DZitem=0 then exit sub
      '
      #define _ALIAS   THIS._dpZoneArrayPtr
      '
      _DYNARRAYINCREASE(_ALIAS)
      _ARRAYLASTITEM(_ALIAS) => DZitem
      '
      #ifdef _ALIAS
         #undef _ALIAS
      #endif
   end sub
   sub DGDPBTN.AddDropZoneArrayOfPtrToArrayOfPtr(DZArrayOfPtr() as DROPZONE ptr)
      if uBound(DZArrayOfPtr)<0 then exit sub
      '
      #define _ALIAS   THIS._dpZoneArrayPtr
      '
      dim as integer   initialLastIndex = uBound(_ALIAS)
      for index as integer = 0 to uBound(DZArrayOfPtr)
         if DZArrayOfPtr(index)<>0 then
            _DYNARRAYINCREASE(_ALIAS)
            _ARRAYLASTITEM(_ALIAS) => DZArrayOfPtr(index)
         end if
      next index
      '
      #ifdef _ALIAS
         #undef _ALIAS
      #endif
   end sub
   sub DGDPBTN.TestScreen()
      screenInfo   THIS._scrW, _ 
               THIS._scrH, _ 
               THIS._scrDepth
      dim as ulong scrWidth   => width()
      THIS._colWidth         => loWord(scrWidth)
      THIS._rowWidth         => hiWord(scrWidth)
   end sub
   function DGDPBTN.IsInButtonShape(byval GmX as integer, _ 
                                    byval GmY as integer) _ 
                                    as boolean
      select case THIS._btnShape
         case DGDPBTN._DGDPBTNSHAPE._naked
            if GmX>=THIS.Xi   andAlso _ 
               GmX<=THIS.Xf   andAlso _ 
               GmY>=THIS.Yi   andAlso _ 
               GmY<=THIS.Yf then
               '---->
               return TRUE
            else
               '---->
               return FALSE
            end if
         case DGDPBTN._DGDPBTNSHAPE._squared
            if GmX>=THIS.Xi - 8   andAlso _ 
               GmX<=THIS.Xf + 8   andAlso _ 
               GmY>=THIS.Yi   - 8 andAlso _ 
               GmY<=THIS.Yf + 8   then
               '---->
               return TRUE
            else
               '---->
               return FALSE
            end if
         case else
         'DGDPBTN._DGDPBTNSHAPE._rounded
            if GmX>=THIS.Xi - THIS._widHei._wid/2      andAlso _
               GmX<=THIS.Xf + THIS._widHei._wid/2      andAlso _
               GmY>=THIS.Yi - THIS._widHei._hei/2      andAlso _
               GmY<=THIS.Yf + THIS._widHei._hei/2      then
               if ( GmX>=THIS.Xi                     andAlso   _ 
                    GmX<=THIS.Xf                     andAlso   _ 
                    GmY>=THIS.Yi - THIS._widHei._hei/2      andAlso   _ 
                    GmY<=THIS.Yf + THIS._widHei._wid/2 )   then
                     '---->
                     return TRUE
               else
                  dim as integer   rx   => THIS.RoundedShapeRadius._bvx
                  dim as integer   ry   => THIS.RoundedShapeRadius._bvy
                  dim as single   yrMinLeft   => THIS.Yi - sqr(rx^2 - ( GmX - THIS.Xi)^2)*ry/rx
                  dim as single   yrMaxLeft   => THIS.Yf + sqr(rx^2 - ( GmX - THIS.Xi)^2)*ry/rx
                  dim as single   yrMinRight   => THIS.Yi - sqr(rx^2 - ( GmX - THIS.Xf)^2)*ry/rx
                  dim as single   yrMaxRight   => THIS.Yf + sqr(rx^2 - ( GmX - THIS.Xf)^2)*ry/rx
                  if ( GmX>=THIS.Xi - THIS._widHei._wid/2      andAlso _
                       GmX<=THIS.Xi                     andAlso _
                       GmY>=yrMinLeft                     andAlso _ 
                       GmY<=yrMaxLeft   )                  orElse   _
                     ( GmX>=THIS.Xf                     andAlso _
                       GmX<=THIS.Xf + THIS._widHei._wid/2      andAlso _
                       GmY>=yrMinRight                  andAlso _ 
                       GmY<=yrMaxRight )                  then
                        '---->
                        return TRUE
                  else
                        '---->
                        return FALSE
                  end if
               end if
            else
               '---->
               return FALSE
            end if
      end select
   end function
   sub DGDPBTN.TestMouse()
      static as boolean   hasMovedAtLeastOnce
      dim as integer   gmX, gmY, gmWheel, gmBtn
      getMouse   gmX, gmY, gmWheel, gmBtn
      '
      #ifdef _DEBUG
         _P(hasMovedAtLeastOnce)
         _P(THIS._hasMouseClick)
         _P(THIS._hasDragStarted)
         _P(THIS._hasMouseOver)
         _P(THIS._isMouseOverDropZone)
         _P(THIS._isMouseDropped)
      #endif
      THIS._hasMultiplicityIndicator  = FALSE
      if not THIS._hasDragStarted then
         for index as integer = lBound(DGDPBTN.arrayOfDragDropButtonPtr) to uBound(DGDPBTN.arrayOfDragDropButtonPtr)
            if THIS._hasMouseOver then
               if DGDPBTN.arrayOfDragDropButtonPtr(index)<>@THIS andAlso DGDPBTN.arrayOfDragDropButtonPtr(index)->_hasMouseOver then
                  THIS._hasMultiplicityIndicator = TRUE
               end if
            end if
         next index
      end if
      for index as integer = lBound(DGDPBTN.arrayOfDragDropButtonPtr) to uBound(DGDPBTN.arrayOfDragDropButtonPtr)
         if THIS._hasMouseClick then
            if DGDPBTN.arrayOfDragDropButtonPtr(index)<>@THIS andAlso DGDPBTN.arrayOfDragDropButtonPtr(index)->_hasMouseClick then
               THIS._hasMouseClick = FALSE
               exit sub
            end if
         end if
      next index
      
      dim as boolean hasAlreadyDragStartedForElse  => FALSE
      for index as integer = lBound(DGDPBTN.arrayOfDragDropButtonPtr) to uBound(DGDPBTN.arrayOfDragDropButtonPtr)
         if DGDPBTN.arrayOfDragDropButtonPtr(index)<>@THIS andAlso DGDPBTN.arrayOfDragDropButtonPtr(index)->_hasDragStarted then
            hasAlreadyDragStartedForElse = TRUE
            exit sub
         end if
      next index
      '
      select case THIS._hasMouseClick
         case TRUE
            THIS._hasDragStarted = TRUE
            '
            if not _ 
               ( gmX=(THIS._topLeftCorner._x + THIS._offsetAtClick._x)   andAlso _ 
                 gmY=(THIS._topLeftCorner._y + THIS._offsetAtClick._y) )   then
                'moving -> update position
                  if not hasMovedAtLeastOnce then hasMovedAtLeastOnce = TRUE
                  
                  if THIS._isDragAllowed then 
                     THIS._topLeftCorner._x   = gmX - THIS._offsetAtClick._x
                     THIS._topLeftCorner._y   = gmY - THIS._offsetAtClick._y
                  else
                     THIS._hasMouseOver = FALSE
                     THIS._hasMouseClick = FALSE
                  end if
            else
               'not moving
               if not hasMovedAtLeastOnce then 
                  THIS._hasDragStarted = FALSE
               else
                  THIS._hasDragStarted = TRUE
               end if
               '
               if THIS.IsInButtonShape(gmX, gmY) then
                  if not gmBtn>0 then
                     'reset mouseClick state
                     if hasMovedAtLeastOnce then hasMovedAtLeastOnce = FALSE
                     THIS._hasMouseClick = FALSE
                     THIS._offsetAtClick._x   = -1
                     THIS._offsetAtClick._y   = -1
                  end if
               else
                  'reset mouseOver state
                  THIS._hasMouseOver = FALSE
               end if
            end if
         case else
            'THIS._hasMouseClick==FALSE
            if THIS._hasDragStarted then
               THIS._hasDragStarted = FALSE
            end if
            '
            if THIS.IsInButtonShape(gmX, gmY) then
               if not THIS._hasMouseOver then THIS._hasMouseOver = TRUE
               '
               if gmBtn>0 then
                  THIS._hasMouseClick = TRUE
                  THIS._offsetAtClick._x   = gmX - THIS._topLeftCorner._x
                  THIS._offsetAtClick._y   = gmY - THIS._topLeftCorner._y
               end if
            else
               if THIS._hasMouseOver then THIS._hasMouseOver = FALSE
            end if
      end select
      '
      if THIS._hasDragStarted then
         'test if dropped if dropability previously granted
         select case THIS._isMouseOverDropZone
            case TRUE
               'is dropable
               if THIS.IsInButtonShape(gmX, gmY) then
                  if not gmBtn>0 then
                     'reset mouseClick state
                     if not THIS._isMouseDropped then
                        '
                        if THIS._dropZoneCandidateindex>-1 andAlso _ 
                           THIS._dpZoneArrayPtr(THIS._dropZoneCandidateindex)->_capacity>THIS._dpZoneArrayPtr(THIS._dropZoneCandidateindex)->_occupiedSlotCount then
                              with *THIS._dpZoneArrayPtr(THIS._dropZoneCandidateindex)
                                 _DYNARRAYINCREASE(._arrayOfDroppedPtr)
                              end with
                              _ARRAYLASTITEM(THIS._dpZoneArrayPtr(THIS._dropZoneCandidateindex)->_arrayOfDroppedPtr)=cInt(@THIS)
                              THIS._isMouseDropped = TRUE
                              THIS._dpZoneArrayPtr(THIS._dropZoneCandidateindex)->_occupiedSlotCount += 1
                        end if
                        '
                        THIS._hasMouseClick = FALSE
                     else
                        '
                     end if
                  else
                     ? "hekk"   
                     if THIS._isMouseDropped then
                        'if THIS found in the array of dropzone, remove it
                        with *THIS._dpZoneArrayPtr(THIS._dropZoneCandidateindex)
                           for index as integer = lBound(._arrayOfDroppedPtr) to uBound(._arrayOfDroppedPtr)
                              'mark invalid slots
                              if ._arrayOfDroppedPtr(index)=cInt(@THIS) then
                                 THIS._dpZoneArrayPtr(THIS._dropZoneCandidateindex)->_occupiedSlotCount -= 1
                                 ._arrayOfDroppedPtr(index) = 0
                              end if
                           next index
                           dim as integer invalidIndexLowerBound = uBound(._arrayOfDroppedPtr)
                           for index as integer = lBound(._arrayOfDroppedPtr) to uBound(._arrayOfDroppedPtr)
                              if index=invalidIndexLowerBound then
                                 exit for
                              end if
                              'move invalid slots to ubound side
                              if ._arrayOfDroppedPtr(index)=0 then
                                 swap ._arrayOfDroppedPtr(index), ._arrayOfDroppedPtr(invalidIndexLowerBound)
                                 invalidIndexLowerBound -= 1
                              end if
                           next index
                           ? invalidIndexLowerBound
                           'remove invalid slots from the array
                           if invalidIndexLowerBound=lBound(._arrayOfDroppedPtr) then
                              erase ._arrayOfDroppedPtr
                           else
                              redim preserve ._arrayOfDroppedPtr(lBound(._arrayOfDroppedPtr) to invalidIndexLowerBound + 1)
                           end if
                        end with
                        THIS._isMouseDropped = FALSE
                     end if 
                  end if
               else
                  'reset mouseOver state
                  THIS._hasMouseOver = FALSE
               end if
            case else
               'is not dropable
               if THIS.IsInButtonShape(gmX, gmY) then
                  if not gmBtn>0 then
                     'reset mouseClick state
                     if THIS._isMouseDropped then THIS._isMouseDropped = FALSE
                     THIS._hasMouseClick = FALSE
                  end if
               else
                  'reset mouseOver state
                  THIS._hasMouseOver = FALSE
               end if
         end select
         'test for dropability
         for index as integer = lBound(THIS._dpZoneArrayPtr) to uBound(THIS._dpZoneArrayPtr)
            '
            #ifDef _DEBUGMODE
            _P(index)
            _P(THIS._dpZoneArrayPtr(index)->IsInDropzoneShape(gmX, gmY))
            #endIF
            '
            if THIS._dpZoneArrayPtr(index)->IsInDropzoneShape(gmX, gmY)   then
               if not THIS._isMouseOverDropZone then THIS._isMouseOverDropZone = TRUE
               THIS._dropZoneCandidateindex  = index
               exit sub
            else
               if THIS._isMouseOverDropZone then THIS._isMouseOverDropZone = FALSE
               THIS._dropZoneCandidateindex  = -1
            end if
         next index
      end if
   end sub
   sub DGDPBTN.ShowAllDropZone()
      #define _ALIAS   THIS._dpZoneArrayPtr
      '
      'get background color
      dim as integer bkgColor
      screenControl fb.GET_COLOR, , bkgColor
      'get screen color depth
      dim as integer   scrDepth
      screenInfo   , , scrDepth
      '
      for index as integer = 0 to uBound(THIS._dpZoneArrayPtr)
         line  (_ALIAS(index)->Xi + 2, _ALIAS(index)->Yi + 2)- _ 
               (_ALIAS(index)->Xf - 2, _ALIAS(index)->Yf - 2), _ 
               bkgColor/scrDepth, _ 
               b, _ 
               &b00000111
      next index
      '
      #ifdef _ALIAS
         #undef _ALIAS
      #endif
   end sub
   sub DGDPBTN.DrawDgdpbtn()
      THIS.TestMouse()
      '
      dim as ulong   btnColour
      if THIS._isMouseOverDropZone      then
         btnColour = THIS._mouseDroppableColour
      elseIf THIS._hasDragStarted         then
         btnColour = THIS._mouseDragColour
      elseIf THIS._hasMouseClick         then
         btnColour = THIS._mouseClickColour
      elseIf THIS._hasMouseOver         then
         btnColour = THIS._mouseOverColour
      elseIf not THIS._isMouseDropped then
         btnColour = THIS._idleColour
      end if
      if not THIS._hasDragStarted  andAlso THIS._isMouseDropped then
         btnColour = btnColour xor THIS._mouseDroppedColour
      end if
      '
      'if THIS._isMouseOverDropZone then THIS.ShowAllDropZone()
      '
      select case THIS._btnShape
         case DGDPBTN._DGDPBTNSHAPE._naked
            line (THIS.Xi, THIS.Yi)-(THIS.Xf, THIS.Yf), btnColour, b
         case DGDPBTN._DGDPBTNSHAPE._squared
            line  (THIS.Xi - 8, THIS.Yi - 8)- _ 
                  (THIS.Xf + 8, THIS.Yf + 8), _ 
                  btnColour, _ 
                  bf
         case else
            dim as integer rx   => THIS.RoundedShapeRadius._bvx
            dim as integer ry   => THIS.RoundedShapeRadius._bvy
            dim as integer rr   => _MAX(THIS.RoundedShapeRadius._bvx, THIS.RoundedShapeRadius._bvy)
            line (THIS.Xi - 1, THIS.Yi - 1)-(THIS.Xf + 1, THIS.Yf + 1), btnColour, bf
            '
            line (THIS.Xi - rx, THIS.Yi)-(THIS.Xi, THIS.Yf), btnColour, bf
            line (THIS.Xi, THIS.Yf + ry)-(THIS.Xf, THIS.Yf), btnColour, bf
            line (THIS.Xf + rx, THIS.Yf)-(THIS.Xf, THIS.Yi), btnColour, bf
            line (THIS.Xf, THIS.Yi - ry)-(THIS.Xi, THIS.Yi), btnColour, bf
            '
            dim as integer   inc => rr
            while inc>=0 
               inc -= 1
            circle (THIS.Xi, THIS.Yi), _ 
                  inc, _ 
                  btnColour, _ 
                  _pi/2, _ 
                  _pi, _
                  THIS.RoundedShapeRadius._bvy/THIS.RoundedShapeRadius._bvx
            circle (THIS.Xi, THIS.Yf), _ 
                  inc, _ 
                  btnColour, _ 
                  _pi, _ 
                  3*_pi/2, _
                  THIS.RoundedShapeRadius._bvy/THIS.RoundedShapeRadius._bvx
            circle (THIS.Xf, THIS.Yf), _ 
                  inc, _ 
                  btnColour, _ 
                  3*_pi/2, _ 
                  _2pi, _
                  THIS.RoundedShapeRadius._bvy/THIS.RoundedShapeRadius._bvx
            circle (THIS.Xf, THIS.Yi), _ 
                  inc, _ 
                  btnColour, _ 
                  0, _ 
                  _pi/2, _
                  THIS.RoundedShapeRadius._bvy/THIS.RoundedShapeRadius._bvx
            wend
      end select
      '
      draw string (THIS.Xi + 4, THIS.Yi + 4), _ 
                  THIS._btnTxt, _ 
                  _COMPLEMENTCOLOR(btnColour, rgba(200,200,160,210))
      '
      if THIS._hasMultiplicityIndicator then
         draw string  (THIS.Xi  - 18, THIS.Yi - 18), "m", btnColour
         line (THIS.Xi  - 18, THIS.Yi - 4)-step(6,0), btnColour
      end if
   end sub
end nameSpace
#macro _DRAWDRAGGABLEBUTTONSARRAY(DGBTNPTRARR)
   dim as integer arrayIndex  => any
   'search for the currently dragged button (if any) and put it at the last array position
   'draw its drop zone by the way
   for arrayIndex = lBound(DGBTNPTRARR) to uBound(DGBTNPTRARR)
      if DGBTNPTRARR(arrayIndex)->_hasMouseClick then
         DGBTNPTRARR(arrayIndex)->ShowAllDropZone
      end if
      if DGBTNPTRARR(arrayIndex)->_hasDragStarted then
         for dpzArrayIndex as integer = lBound(DGBTNPTRARR(arrayIndex)->_dpZoneArrayPtr) to uBound(DGBTNPTRARR(arrayIndex)->_dpZoneArrayPtr)
            DGBTNPTRARR(arrayIndex)->_dpZoneArrayPtr(dpzArrayIndex)->DrawDropZone
         next dpzArrayIndex
         if not arrayIndex=uBound(DGBTNPTRARR) then
            swap DGBTNPTRARR(uBound(DGBTNPTRARR)), DGBTNPTRARR(arrayIndex)
         end if
      end if
   next arrayIndex
   'draw the array of draggable buttons
   for arrayIndex = lBound(DGBTNPTRARR) to uBound(DGBTNPTRARR)
      DGBTNPTRARR(arrayIndex)->DrawDgdpbtn()
   next arrayIndex
#endMacro

nameSpace ttBtn
   type DBUTTON
   public:
      enum _BTNBEHAVIOUR
         _useDelay   = -1
         _standard   = 0
      end enum '_BTNBEHAVIOUR
      declare constructor()
      declare constructor( byval as integer, _
                           byval as integer, _
                           byval as string   )
      declare constructor( byval as integer, _
                           byval as integer, _
                           byval as integer, _
                           byval as integer, _
                           byval as string, _
                           byval as _BTNBEHAVIOUR=_BTNBEHAVIOUR._useDelay, _
                           byval as double=0.5  )
      declare property Behaviour() as _BTNBEHAVIOUR
      declare property Behaviour(byval as _BTNBEHAVIOUR)
      declare property ClickTimeInterval() as double   
      declare property ClickTimeInterval(byval as double)
      declare property LastClickTime() as double
      declare sub TestButton()
      declare sub DrawButton()
         as integer         _topLeftCornerX
         as integer         _topLeftCornerY
         as integer         _bottomRightCornerX
         as integer         _bottomRightCornerY
         as string          _text
         as boolean         _mouseOver
         as boolean         _mouseClick
         as boolean         _mouseLegalIntervalClick
         as boolean         _isQuiet
         '
         as ulong          _btnBorderColor
         as ulong          _btnBorderMouseOverColor
         as ulong          _btnBorderMouseClickColor
         as ulong          _btnColor
         as ulong          _btnMouseOverColor
         as ulong          _btnMouseClickColor
         as ulong          _btnTxtColor
   private:
         as _BTNBEHAVIOUR   _behaviour
         as double          _lastClickedTime
         as double          _minClickTimeInterval
   end type 'DBUTTON
   constructor DBUTTON()
      dim as integer scrW, scrH
      if screenPtr()=0 then
         scrW = 200
         scrH = 200
      else
         screenInfo scrW, scrH
      end if
      '
      with THIS
         ._text              => "DBUTTON"
         ._topLeftCornerX    => (scrW - 8*len(._text))\2
         ._topLeftCornerY    => (scrH - 20)\2
         ._bottomRightCornerX=> (scrW - 8*len(._text))\2 + 56
         ._bottomRightCornerY=> (scrH + 10)\2
         ._mouseOver       => FALSE
         ._mouseClick      => FALSE
         ._lastClickedTime         => 0
         ._behaviour               => DBUTTON._BTNBEHAVIOUR._standard
         ._minClickTimeInterval      => 0.5
         ._mouseLegalIntervalClick   => FALSE
      end with 'THIS
      with THIS
         ._btnBorderColor              => rgba(0,100,100,100)  
         ._btnBorderMouseOverColor     => rgba(0,100,100,100)
         ._btnBorderMouseClickColor    => rgba(0,100,100,100)
         ._btnColor                    => rgba(0,100,100,100)
         ._btnMouseOverColor           => rgba(0,100,100,100)
         ._btnMouseClickColor          => rgba(0,100,100,100)
         ._btnTxtColor                 => rgba(100,100,200,200)
      end with
   end constructor 'DBUTTON explicit default constructor
   constructor DBUTTON( byval TLCX as integer, _
                        byval TLCY as integer, _
                        byval Text as string )
      with THIS
         ._text              => Text
         ._topLeftCornerX    => TLCX
         ._topLeftCornerY    => TLCY
         ._bottomRightCornerX=> ._topLeftCornerX + 8*len(._text)
         ._bottomRightCornerY=> ._topLeftCornerY + 15
         ._mouseOver       => FALSE
         ._mouseClick      => FALSE
         ._lastClickedTime          => 0
         ._behaviour                => DBUTTON._BTNBEHAVIOUR._standard
         ._minClickTimeInterval     => 0.5
         ._mouseLegalIntervalClick  => FALSE
      end with 'THIS
      with THIS
         ._btnBorderColor              => rgba(100,100,100,100)  
         ._btnBorderMouseOverColor     => rgba(100,100,100,100)
         ._btnBorderMouseClickColor    => rgba(100,100,100,100)
         ._btnColor                    => rgba(100,100,100,100)
         ._btnMouseOverColor           => rgba(100,100,100,100)
         ._btnMouseClickColor          => rgba(100,100,100,100)
         ._btnTxtColor                 => rgba(100,100,200,200)
      end with
   end constructor 'DBUTTON(valINT,valINT,valSTR)
   constructor DBUTTON( byval TLCX as integer, _
                        byval TLCY as integer, _
                        byval BtnWidth as integer, _
                        byval BtnHeight as integer, _
                        byval Text as string, _,
                        byval BtnBehaviour as DBUTTON._BTNBEHAVIOUR=-1, _
                        byval CTI as double=0.5 )
      if BtnHeight<15 then BtnHeight = 15
      with THIS
         ._topLeftCornerX    => TLCX
         ._topLeftCornerY    => TLCY
         ._bottomRightCornerX=> ._topLeftCornerX + BtnWidth
         ._bottomRightCornerY=> ._topLeftCornerY + BtnHeight
         ._text            => left(Text, BtnWidth\8)
         ._mouseOver       => FALSE
         ._mouseClick      => FALSE
         ._lastClickedTime         => 0
         ._behaviour               => DBUTTON._BTNBEHAVIOUR._standard
         ._minClickTimeInterval      => CTI
         ._mouseLegalIntervalClick   => FALSE
      end with 'THIS
      with THIS
         ._btnBorderColor              => rgba(100,100,100,100)  
         ._btnBorderMouseOverColor     => rgba(100,100,100,100)
         ._btnBorderMouseClickColor    => rgba(100,100,100,100)
         ._btnColor                    => rgba(120,120,100,120)
         ._btnMouseOverColor           => rgba(100,140,100,220)
         ._btnMouseClickColor          => rgba(100,100,100,120)
         ._btnTxtColor                 => rgba(100,70,120,  200)
      end with
   end constructor 'DBUTTON(valINT,valINT,valINT,valINT,valSTR)
   property DBUTTON.Behaviour() as DBUTTON._BTNBEHAVIOUR
      '---->
      return THIS._behaviour
   end property 'get BUTTON_BTNBEHAVIOUR:=DBUTTON.Behaviour
   property DBUTTON.Behaviour(byval SetValue as DBUTTON._BTNBEHAVIOUR)
      THIS._behaviour = SetValue
   end property 'set DBUTTON.Behaviour(valBUTTON_BTNBEHAVIOUR)
   property DBUTTON.ClickTimeInterval() as double
      '---->
      return THIS._minClickTimeInterval
   end property 'get DBL:=DBUTTON.ClickTimeInterval   
   property DBUTTON.ClickTimeInterval(byval SetValue as double)
      THIS._minClickTimeInterval = SetValue
   end property 'set DBUTTON.ClickTimeInterval(valDBL)
   property DBUTTON.LastClickTime() as double
      '---->
      return THIS._lastClickedTime
   end property 'get DBL:=DBUTTONLastClickTime
   sub DBUTTON.TestButton()
      dim as integer gmX, gmY, gmBtn1
      getMouse gmX, gmY, , gmBtn1
      '
      with THIS
         if gmX>._topLeftCornerX and _
            gmY>._topLeftCornerY and _
            gmX<._bottomRightCornerX and _
            gmY<._bottomRightCornerY then
            if ._mouseOver=FALSE then ._mouseOver = TRUE
            if gmBtn1=+1 then
               if ._mouseClick=FALSE then '._mouseClick = TRUE
                  if (TIMER - ._lastClickedTime)>._minClickTimeInterval then
                     ._mouseClick = TRUE
                     THIS._lastClickedTime = TIMER
                     if ._mouseLegalIntervalClick=FALSE then _
                                    ._mouseLegalIntervalClick = TRUE
                     else
                     if ._mouseLegalIntervalClick=TRUE then _
                                    ._mouseLegalIntervalClick = FALSE
                  end if
               else
                  if ._mouseClick=TRUE then ._mouseClick = FALSE
                  if ._mouseLegalIntervalClick=TRUE then _
                                                ._mouseLegalIntervalClick = FALSE
               end if
            end if
         else
            if ._mouseOver=TRUE then ._mouseOver = FALSE
            if ._mouseClick=TRUE then ._mouseClick = FALSE
            if ._mouseLegalIntervalClick=TRUE then _
                                             ._mouseLegalIntervalClick = FALSE
         end if
      end with 'THIS
   end sub 'DBUTTON.TestButton()
   sub DBUTTON.DrawButton()
      /'
         ._btnBorderColor
         ._btnBorderMouseOverColor
         ._btnBorderMouseClickColor
         ._btnColor
         ._btnMouseOverColor
         ._btnMouseClickColor
      '/
      dim as ulong   btnColor
      with THIS
         .TestButton()
         if ._mouseClick=TRUE then
            if ._behaviour=-1 then
               btnColor = ._btnColor xor rgba(255,180,140, 100)
            else
               btnColor = _btnColor
            end if
         elseif ._mouseOver=TRUE then
            if (TIMER - ._lastClickedTime)<._minClickTimeInterval and _
                                                   ._behaviour=-1 then
               btnColor = ._btnMouseClickColor xor rgba(180,140,120, 100)
            else
               btnColor = _btnMouseClickColor
            end if
         else
            if (TIMER - ._lastClickedTime)<._minClickTimeInterval and _
                                                   ._behaviour=-1 then
               if not ._isQuiet then
                  btnColor = ._btnMouseOverColor xor rgba(100,180,240, 100)
               end if
            else
               if not ._isQuiet then
                  btnColor = _btnMouseOverColor
               end if
            end if
         end if
         '
         if not ._isQuiet then
            line (   ._topLeftCornerX, ._topLeftCornerY)-_
                     (._bottomRightCornerX, ._bottomRightCornerY  ), _
                  btnColor, _
                  bf
         else
            if btnColor=0 then
               line (   ._topLeftCornerX + 3, ._topLeftCornerY + 3)-_
                        (._bottomRightCornerX - 1, ._bottomRightCornerY - 1   ), _
                     0, _
                     bf
            else
               line (   ._topLeftCornerX, ._topLeftCornerY)-_
                        (._bottomRightCornerX, ._bottomRightCornerY), _
                     btnColor, _
                     bf
            end if
         end if
         draw string (  ._topLeftCornerX + 1 + 12, ._topLeftCornerY - 1 + 6 + _
                        0*(._bottomRightCornerY - _topLeftCornerY)\2   ), _
                     left(._text, (._bottomRightCornerX - ._topLeftCornerX)), _
                     ._btnTxtColor or btnColor, _ 
                     starter
         line (   ._topLeftCornerX, ._topLeftCornerY)-_
                  (._bottomRightCornerX, ._bottomRightCornerY), _
               ._btnBorderColor, _
               b
      end with 'THIS
   end sub 'DBUTTON.DrawButton()
end nameSpace

nameSpace TT2
   '3D stuff here is borrowed from BasicCode2@fb.net, far too mixed up to dedicate a namespace
   type DBUTTON
   public:
      enum _BTNBEHAVIOUR
         _useDelay   = -1
         _standard   = 0
      end enum '_BTNBEHAVIOUR
      declare constructor()
      declare constructor( byval as integer, _
                           byval as integer, _
                           byval as string   )
      declare constructor( byval as integer, _
                           byval as integer, _
                           byval as integer, _
                           byval as integer, _
                           byval as string, _
                           byval as _BTNBEHAVIOUR=_BTNBEHAVIOUR._useDelay, _
                           byval as double=0.5  )
      declare property Behaviour() as _BTNBEHAVIOUR
      declare property Behaviour(byval as _BTNBEHAVIOUR)
      declare property ClickTimeInterval() as double   
      declare property ClickTimeInterval(byval as double)
      declare property LastClickTime() as double
      declare sub TestButton()
      declare sub DrawButton()
         as integer         _topLeftCornerX
         as integer         _topLeftCornerY
         as integer         _bottomRightCornerX
         as integer         _bottomRightCornerY
         as string          _text
         as boolean         _mouseOver
         as boolean         _mouseClick
         as boolean         _mouseLegalIntervalClick
         as boolean         _isQuiet
         '
         as ulong          _btnBorderColor
         as ulong          _btnBorderMouseOverColor
         as ulong          _btnBorderMouseClickColor
         as ulong          _btnColor
         as ulong          _btnMouseOverColor
         as ulong          _btnMouseClickColor
         as ulong          _btnTxtColor
   private:
         as _BTNBEHAVIOUR   _behaviour
         as double          _lastClickedTime
         as double          _minClickTimeInterval
   end type 'DBUTTON
   constructor DBUTTON()
      dim as integer scrW, scrH
      if screenPtr()=0 then
         scrW = 200
         scrH = 200
      else
         screenInfo scrW, scrH
      end if
      '
      with THIS
         ._text              => "DBUTTON"
         ._topLeftCornerX    => (scrW - 8*len(._text))\2
         ._topLeftCornerY    => (scrH - 20)\2
         ._bottomRightCornerX=> (scrW - 8*len(._text))\2 + 56
         ._bottomRightCornerY=> (scrH + 10)\2
         ._mouseOver       => FALSE
         ._mouseClick      => FALSE
         ._lastClickedTime         => 0
         ._behaviour               => DBUTTON._BTNBEHAVIOUR._standard
         ._minClickTimeInterval      => 0.5
         ._mouseLegalIntervalClick   => FALSE
      end with 'THIS
      with THIS
         ._btnBorderColor              => rgba(0,100,100,100)  
         ._btnBorderMouseOverColor     => rgba(0,100,100,100)
         ._btnBorderMouseClickColor    => rgba(0,100,100,100)
         ._btnColor                    => rgba(0,100,100,100)
         ._btnMouseOverColor           => rgba(0,100,100,100)
         ._btnMouseClickColor          => rgba(0,100,100,100)
         ._btnTxtColor                 => rgba(100,100,200,200)
      end with
   end constructor 'DBUTTON explicit default constructor
   constructor DBUTTON( byval TLCX as integer, _
                        byval TLCY as integer, _
                        byval Text as string )
      with THIS
         ._text              => Text
         ._topLeftCornerX    => TLCX
         ._topLeftCornerY    => TLCY
         ._bottomRightCornerX=> ._topLeftCornerX + 8*len(._text)
         ._bottomRightCornerY=> ._topLeftCornerY + 15
         ._mouseOver       => FALSE
         ._mouseClick      => FALSE
         ._lastClickedTime          => 0
         ._behaviour                => DBUTTON._BTNBEHAVIOUR._standard
         ._minClickTimeInterval     => 0.5
         ._mouseLegalIntervalClick  => FALSE
      end with 'THIS
      with THIS
         ._btnBorderColor              => rgba(100,100,100,100)  
         ._btnBorderMouseOverColor     => rgba(100,100,100,100)
         ._btnBorderMouseClickColor    => rgba(100,100,100,100)
         ._btnColor                    => rgba(100,100,100,100)
         ._btnMouseOverColor           => rgba(100,100,100,100)
         ._btnMouseClickColor          => rgba(100,100,100,100)
         ._btnTxtColor                 => rgba(100,100,200,200)
      end with
   end constructor 'DBUTTON(valINT,valINT,valSTR)
   constructor DBUTTON( byval TLCX as integer, _
                        byval TLCY as integer, _
                        byval BtnWidth as integer, _
                        byval BtnHeight as integer, _
                        byval Text as string, _,
                        byval BtnBehaviour as DBUTTON._BTNBEHAVIOUR=-1, _
                        byval CTI as double=0.5 )
      if BtnHeight<15 then BtnHeight = 15
      with THIS
         ._topLeftCornerX    => TLCX
         ._topLeftCornerY    => TLCY
         ._bottomRightCornerX=> ._topLeftCornerX + BtnWidth
         ._bottomRightCornerY=> ._topLeftCornerY + BtnHeight
         ._text            => left(Text, BtnWidth\8)
         ._mouseOver       => FALSE
         ._mouseClick      => FALSE
         ._lastClickedTime         => 0
         ._behaviour               => DBUTTON._BTNBEHAVIOUR._standard
         ._minClickTimeInterval      => CTI
         ._mouseLegalIntervalClick   => FALSE
      end with 'THIS
      with THIS
         ._btnBorderColor              => rgba(100,100,100,100)  
         ._btnBorderMouseOverColor     => rgba(100,100,100,100)
         ._btnBorderMouseClickColor    => rgba(100,100,100,100)
         ._btnColor                    => rgba(120,120,100,120)
         ._btnMouseOverColor           => rgba(100,140,100,220)
         ._btnMouseClickColor          => rgba(100,100,100,120)
         ._btnTxtColor                 => rgba(100,70,120,  200)
      end with
   end constructor 'DBUTTON(valINT,valINT,valINT,valINT,valSTR)
   property DBUTTON.Behaviour() as DBUTTON._BTNBEHAVIOUR
      '---->
      return THIS._behaviour
   end property 'get BUTTON_BTNBEHAVIOUR:=DBUTTON.Behaviour
   property DBUTTON.Behaviour(byval SetValue as DBUTTON._BTNBEHAVIOUR)
      THIS._behaviour = SetValue
   end property 'set DBUTTON.Behaviour(valBUTTON_BTNBEHAVIOUR)
   property DBUTTON.ClickTimeInterval() as double
      '---->
      return THIS._minClickTimeInterval
   end property 'get DBL:=DBUTTON.ClickTimeInterval   
   property DBUTTON.ClickTimeInterval(byval SetValue as double)
      THIS._minClickTimeInterval = SetValue
   end property 'set DBUTTON.ClickTimeInterval(valDBL)
   property DBUTTON.LastClickTime() as double
      '---->
      return THIS._lastClickedTime
   end property 'get DBL:=DBUTTONLastClickTime
   sub DBUTTON.TestButton()
      dim as integer gmX, gmY, gmBtn1
      getMouse gmX, gmY, , gmBtn1
      '
      with THIS
         if gmX>._topLeftCornerX and _
            gmY>._topLeftCornerY and _
            gmX<._bottomRightCornerX and _
            gmY<._bottomRightCornerY then
            if ._mouseOver=FALSE then ._mouseOver = TRUE
            if gmBtn1=+1 then
               if ._mouseClick=FALSE then
                  if (TIMER - ._lastClickedTime)>._minClickTimeInterval then
                      ._mouseClick = TRUE
                     THIS._lastClickedTime = TIMER
                     if ._mouseLegalIntervalClick=FALSE then _
                                    ._mouseLegalIntervalClick = TRUE
                  else
                     if ._mouseLegalIntervalClick=TRUE then _
                                    ._mouseLegalIntervalClick = FALSE
                  end if
               else
                  if ._mouseClick=TRUE then ._mouseClick = FALSE
                  if ._mouseLegalIntervalClick=TRUE then _
                                                ._mouseLegalIntervalClick = FALSE
               end if
            end if
         else
            if ._mouseOver=TRUE then ._mouseOver = FALSE
            if ._mouseClick=TRUE then ._mouseClick = FALSE
            if ._mouseLegalIntervalClick=TRUE then _
                                             ._mouseLegalIntervalClick = FALSE
         end if
      end with 'THIS
   end sub 'DBUTTON.TestButton()
   sub DBUTTON.DrawButton()
      /'
         ._btnBorderColor
         ._btnBorderMouseOverColor
         ._btnBorderMouseClickColor
         ._btnColor
         ._btnMouseOverColor
         ._btnMouseClickColor
      '/
      dim as ulong   btnColor
      with THIS
         .TestButton()
         if ._mouseClick=TRUE then
            if ._behaviour=-1 then
               btnColor = ._btnColor xor rgba(255,180,140, 100)
            else
               btnColor = _btnColor
            end if
         elseif ._mouseOver=TRUE then
            if (TIMER - ._lastClickedTime)<._minClickTimeInterval and _
                                                   ._behaviour=-1 then
               btnColor = ._btnMouseClickColor xor rgba(180,140,120, 100)
            else
               btnColor = _btnMouseClickColor
            end if
         else
            if (TIMER - ._lastClickedTime)<._minClickTimeInterval and _
                                                   ._behaviour=-1 then
               if not ._isQuiet then
                  btnColor = ._btnMouseOverColor xor rgba(100,180,240, 100)
               end if
            else
               if not ._isQuiet then
                  btnColor = _btnMouseOverColor
               end if
            end if
         end if
         '
         if not ._isQuiet then
            line (   ._topLeftCornerX, ._topLeftCornerY)-_
                     (._bottomRightCornerX, ._bottomRightCornerY  ), _
                  btnColor, _
                  bf
         else
            if btnColor=0 then
               line (   ._topLeftCornerX + 3, ._topLeftCornerY + 3)-_
                        (._bottomRightCornerX - 1, ._bottomRightCornerY - 1   ), _
                     0, _
                     bf
            else
               line (   ._topLeftCornerX, ._topLeftCornerY)-_
                        (._bottomRightCornerX, ._bottomRightCornerY), _
                     btnColor, _
                     bf
            end if
         end if
         draw string (  ._topLeftCornerX + 1, ._topLeftCornerY - 1 + _
                        (._bottomRightCornerY - _topLeftCornerY)\2   ), _
                     left(._text, (._bottomRightCornerX - ._topLeftCornerX)), _
                     ._btnTxtColor or btnColor
         line (   ._topLeftCornerX, ._topLeftCornerY)-_
                  (._bottomRightCornerX, ._bottomRightCornerY), _
               ._btnBorderColor, _
               b
      end with 'THIS
   end sub 'DBUTTON.DrawButton()

   dim shared as ulong  _maxDots  => 178
   #ifNdef _pi
      const as double   _pi       => 4*atn(1)
   #endIf
   const as double      _2pi      => 2*_pi
   const as double      _rToD     => 180/_pi
   const as double      _dToR     => _pi/180

   #define _up      <,>
   #define _down   >,<
   #macro _SETQSORT(datatype, fname, b1, b2, dot)
       sub fname(array() As datatype, Begin as long, Finish as long)
          dim as long i   => Begin, _
                   j   => Finish
          dim as datatype   x => array(((i + j)\2))
          while  (i<=j)
             while array(i)dot b1 x dot : i += 1 : wend
              while array(j)dot b2 x dot : j -= 1 : wend
             if i<=j then swap array(i), array(j): i += 1 : j -= 1
          wend
          if j>Begin   then   fname(array(), Begin, j)
          if i<Finish   then   fname(array(), i, Finish)
       end sub
   #endmacro

   type SORTABLEP3D
       as double   _x
       as double   _y
       as double   _z
       as ulong   _c
       as integer   _id
   end type : _SETQSORT(SORTABLEP3D, Qsort_z, _down, ._z)

   type GENETICUNIT extends DBUTTON
      declare constructor()
         as SORTABLEP3D ptr      _sp3DPtr
         as integer              _id
   end type
   constructor GENETICUNIT()
      BASE()
   end constructor

   type ANGULAR
      const as double   _degreeToRadianCoefficient => _dToR
      enum _ANGLEUNIT
         _rad   => 0
         _deg   => 1
      end enum
      declare constructor()
      declare constructor(byval as double)
      declare constructor(byval AngleInAngleUnit as double=0, _
                     byval MinValueInAngleUnit as double, _
                     byval MaxValueInAngleUnit as double, _
                     byval AngleUnit as _ANGLEUNIT=1)
      declare operator Let(byval as double)
      declare operator Cast() as double
      declare property MinDegreeValue() as double
      declare property MinDegreeValue(byval as double)
      declare property MaxDegreeValue() as double
      declare property MaxDegreeValue(byval as double)
      declare property CurrentDegreeValue() as double
      declare function IncrementValueByDegreeUnit(byval as double=+1) as double
         as double   _angRad
      private:
         as double   _minDegValue
         as double   _maxDegValue
   end type
   constructor ANGULAR()
      THIS._angRad      => 0
      THIS._minDegValue   => 0
      THIS._minDegValue   => 359
   end constructor
   constructor ANGULAR(byval AngleInRad as double)
      THIS._angRad   => AngleInRad
   end constructor
   constructor ANGULAR(byval AngleInAngleUnit as double=0, _
                  byval MinValueInAngleUnit as double, _
                  byval MaxValueInAngleUnit as double, _
                  byval AngleUnit as _ANGLEUNIT=1)
      select case AngleUnit
         case ANGULAR._ANGLEUNIT._rad
            THIS._angRad      => AngleInAngleUnit
            THIS._minDegValue   => MinValueInAngleUnit*1/ANGULAR._degreeToRadianCoefficient
            THIS._maxDegValue   => MinValueInAngleUnit*1/ANGULAR._degreeToRadianCoefficient
         case else
            THIS._angRad      => AngleInAngleUnit*ANGULAR._degreeToRadianCoefficient
            THIS._minDegValue   => MinValueInAngleUnit
            THIS._maxDegValue   => MaxValueInAngleUnit
      end select
   end constructor
   operator ANGULAR.Let(byval LetValue as double)
      THIS._angRad   => LetValue
   end operator
   operator ANGULAR.Cast() as double
      '---->
      return THIS._angRad
   end operator
   property ANGULAR.MinDegreeValue() as double
      if THIS._minDegValue>THIS._maxDegValue then
         swap THIS._minDegValue, THIS._maxDegValue
      end if
      '---->
      return THIS._minDegValue
   end property
   property ANGULAR.MinDegreeValue(byval SetValue as double)
      THIS._minDegValue = SetValue
   end property
   property ANGULAR.MaxDegreeValue() as double
      if THIS._minDegValue>THIS._maxDegValue then
         swap THIS._minDegValue, THIS._maxDegValue
      end if
      '---->
      return THIS._maxDegValue
   end property
   property ANGULAR.MaxDegreeValue(byval SetValue as double)
      THIS._maxDegValue = SetValue
   end property
   property ANGULAR.CurrentDegreeValue() as double
      '---->
      return THIS._angRad*ANGULAR._degreeToRadianCoefficient
   end property
   function ANGULAR.IncrementValueByDegreeUnit(byval IncValue as double=+1) as double
      THIS._angRad += IncValue*1/ANGULAR._degreeToRadianCoefficient
      '
      '---->
      return THIS._angRad
   end function

   type ROTATABLESPHERESET
      declare constructor()
      declare constructor(byval MaxDot as integer, _
                     byval Radius as integer, _
                     byval Angle1 as integer, _
                     byval Angle2 as integer)
      declare property X2D() as integer
      declare property Y2D() as integer
      declare sub SimpleRotationByAngleInDegree(byval as ANGULAR=90)
      'declare sub RotateByAngleInDegree(byval as double=90, byval as double=90)
         as uByte      _maxDot
         as SORTABLEP3D   _sp3dSphereCenter
         as double      _mainSphereRadius
         as double      _individualItemRadius
         as SORTABLEP3D   _absolute3DItemPosition(any)
         as SORTABLEP3D   _relative3DItemPosition(any)
         as GENETICUNIT   _gItemUnit(any)
         as ANGULAR      _angle
         as ANGULAR      _angle1
         as ANGULAR      _angle2
         as double      _projection2DX
         as double      _projection2DY
         '
         as double      _x
         as double      _y
         as double      _z
         as double      _rx
         as double      _ry
         as double      _rz
         as double      _cosAngle
         as double      _sinAngle
         as double      _aRot
   end type
   type RSS   as ROTATABLESPHERESET

   sub ShowActiveAdnNodeBox(  byval ActiveIndex as integer, _
                              GI() as GENETICUNIT, _
                              byval SW as integer, _
                              byval SH as integer  )
      static as integer   _activeIndex          => -1
      static as integer   _processGraphicsStep  => -1
      '
      if _activeIndex<>ActiveIndex then
         /'new value'/
         _activeIndex = ActiveIndex
         _processGraphicsStep = 0
      end if
      if _processGraphicsStep=-1 then exit sub
      '- - - - - - - - - - - - - - - - - - - - - - - - -
      dim as integer x = GI(_activeIndex)._topLeftCornerX
      dim as integer y = GI(_activeIndex)._topLeftCornerY
      select case _processGraphicsStep
         case is<100
            line(x + 2,y + 2)-step(16,16), rgb(055,105,055 + _processGraphicsStep), b
            line(x,y)-step(20 - 80\(_processGraphicsStep + 2),20 - 80\(_processGraphicsStep + 2)), _
               rgb(055,205,155 + _processGraphicsStep), _
               b
            circle (x + 10, y + 10), 20, _
                  rgb(55,100 + _processGraphicsStep,180), _
                  _processGraphicsStep, _
                  _processGraphicsStep + 1
            draw string (x + 2, y + 8), _
                     str(ActiveIndex), _
                     rgb(55,100, 120 + _processGraphicsStep), _ 
                     starter
         case is>=100 , is<150
            circle (x + 10, y + 10), _
                  1980\_processGraphicsStep, _
                  rgb(55, 200 ,80 + _processGraphicsStep), _
                  _processGraphicsStep, _
                  _processGraphicsStep + 2
            draw string (x + 3, y + 8), _
                     str(ActiveIndex), _
                     rgb(55, _processGraphicsStep,180), _ 
                     starter3
      end select
      '
      if _processGraphicsStep>-1      then _processGraphicsStep += 1
      if _processGraphicsStep>150     then _processGraphicsStep = -1
   end sub
end nameSpace

nameSpace USERACTUATOR
   dim shared as fb.IMAGE ptr    up_arrow, down_arrow, left_arrow, right_arrow, mouse_bip, mouse_free
   function InitImage() as boolean
      up_arrow       => CreditFBDoc.BMPlOAD(exePath() & "\pic\up_arrow.bmp")
      down_arrow     => CreditFBDoc.BMPlOAD(exePath() & "\pic\down_arrow.bmp")
      left_arrow     => CreditFBDoc.BMPlOAD(exePath() & "\pic\left_arrow.bmp")
      right_arrow    => CreditFBDoc.BMPlOAD(exePath() & "\pic\right_arrow.bmp")
      mouse_bip      => CreditFBDoc.BMPlOAD(exePath() & "\pic\mouse_bip.bmp")
      mouse_free     => CreditFBDoc.BMPlOAD(exePath() & "\pic\mouse_free.bmp")
      '
      return TRUE
   end function
   '
   function ShowActuator(byval Img as fb.IMAGE ptr, byval OffsetX as integer) as any ptr
      if Img=0 then
         return Img
      end if
      dim as integer imgW, imgH
      imageInfo Img
      put (2 + OffsetX, 3), Img, PSET
      '
      return Img
   end function
end nameSpace

'the game divides in 6 possible scenes
type SCENE
   enum _STATE
      _gameStart        = 0
      _summary          = 1
      _laboratory       = 2
      _travelRoom       = 3
      _groundScenery    = 4
      _endOfGame        = 5
      _gameExit         = 6
   end enum
      as integer  dummy
   static as _STATE  state
end type
dim as SCENE._STATE  SCENE.state => SCENE._STATE._gameStart

'-------------------------------------------------SCEN--------------------------
#define _PLAY
#macro GARBAGE_SCENE
   cls 
   'framework
   summary_LabBtn.DrawButton()
   summary_VesselBtn.DrawButton()
   summary_QuitBtn.DrawButton()
   '...

   'scene rendering---------------------------------------------------------- 
   _DRAWDRAGGABLEBUTTONSARRAY(draggableButtonPtrArray)
   '...
#endMacro

#macro _GAMESTARTPAGE()
   screenSet 0, 1
      scope
         'dim as integer dskW, dskH
         'screenControl fb.GET_DESKTOP_SIZE, dskW, dskH
         '
         djRedundo.fbs_Play_Wave(testWave,1)
         djRedundo.fbs_Play_Wave(testWav2,1)

         '
         dim as integer imgW, imgH
         imageInfo titlepic, imgW, imgH
         '
         imageDestroy titlepic
         '
         titlepic = CreditFBDoc.BMPlOAD(exePath() & "\pic\title_" & trim(str(4)) & ".bmp")
         put ((dskW - imgW)\2, (dskH - imgH)\2), titlepic, TRANS
         put ((dskW - imgW - 340)\2, (dskH - imgH - 60)\2), bioPic, TRANS
         flip
         '
         sleep 3600
         '
         for r as integer = 1 to 20
            circle ((dskW)\2 + 60*cos(2*r), (dskH)\2 + 60*sin(2*r)), 8, rgba(100 + 5*r,100,220,128 - r/4), , , , f
            sleep 20
            '
            flip
         next r
      end scope
   '
   summaryStartTime  = TIMER
   SCENE.state = SCENE._STATE._summary
#endMacro

#macro _SUMMARYPAGE()
   screenSet 0, 1
      cls
      '
      for i as integer = 0 to 15
         line (0, 0 + i*dskH\15)-step(dskW - 1, dskH - 1), rgba(0,50 + i*4,85, 220), bf   
      next i
      line (0, 0)-step(dskW - 1, 40), rgba(20,30,65, 120), bf
      line ((dskW - 390)/2, 280)-step(400,400), rgba(50,250,255, 20), bf
      for i as integer = 0 to 5
         line ((dskW - 390)/2, 280 + i * 24)-step(400,400 - i*24), rgba(50,250 - i*4,55, 20), bf
      next i
      
      'summary menu interface
      summary_LabBtn.DrawButton()
      summary_VesselBtn.DrawButton()
      summary_QuitBtn.DrawButton()
      
      'test for mouseover button
      if summary_LabBtn._mouseOver then
         scope
            dim as integer imgW, imgH
            imageInfo biolitpic, imgW, imgH
            put (40, dskH - imgH*0.88), biolitpic, TRANS
         end scope 
         put ((dskW - 360)/2,300), biohazardpic, trans
         line (2,dskH - 75)-(dskW - 2, dskH - 2), rgba(20,20,20,220), bf
         draw string (10, dskH - 64), "GENETICS FACILITY", 0, starter2        
      elseIf summary_VesselBtn._mouseOver then
         scope
            dim as integer imgW, imgH
            imageInfo biolitpic, imgW, imgH
            put (40, dskH - imgH*0.88), biolitpic, TRANS
         end scope 
         put ((dskW - 360)/2,300), hangarPic, trans
         line (2,dskH - 75)-(dskW - 2, dskH - 2), rgba(20,20,20,220), bf
         draw string (10, dskH - 64), "CREW HANGARS", 0, starter2
      elseIf summary_QuitBtn._mouseOver then
         scope
            dim as integer imgW, imgH
            imageInfo biolitpic, imgW, imgH
            put (40, dskH - imgH*0.88), biolitpic, TRANS
         end scope 
         put ((dskW - 360)/2,300), quitPic, trans
         line (2,dskH - 75)-(dskW - 2, dskH - 2), rgba(20,20,20,220), bf
         draw string (10, dskH - 64), "QUIT", 0, starter2
      else
         if (TIMER - summaryStartTime)>=1 then
            scope
               dim as integer imgW, imgH
               imageInfo biodarkpic, imgW, imgH
               put (40, dskH - imgH*0.88), biodarkpic, TRANS
            end scope
            line (2,dskH - 75)-(dskW - 2, dskH - 2), rgba(20,20,20,120), bf
         end if 
      end if
      
      'test for mouseclick button
      if summary_LabBtn._mouseClick then
         put ((dskW - 360)/2,300), biohazardpic, pset
         line (2,dskH - 75)-(dskW - 2, dskH - 2), rgba(20,20,20,220), bf
         draw string (10, dskH - 64), "GENETICS FACILITY", 0, starter3
         screenCopy 0, 1
         '
         SCENE.state    = SCENE._STATE._laboratory
         djRedundo.fbs_Play_Wave(testWav2,1)
         for i as integer = 0 to 18
            'circle (100, dskH - 220), 22 + i*2 mod 38, rgba(10 + 5*i,100,220,38 + 4*i)
            circle (100 + 40*cos(2*_pi\(i + 1) + _pi), dskH - 220 - 40*sin(2*_pi\(i + 1)) + _pi), 4, rgba(80 + 5*i,100,220,128), , , , f
            flip
            sleep 40
         next i
         sleep 600
         '
         'init lab scene
         adnMakeUpParam       = 0       
         adnMakesUpStartTime  = -1
         adnMakesUp  = FALSE
         adnShowUp   = FALSE
         labStartTime  = TIMER
         '
      end if
      if summary_VesselBtn._mouseClick then
         put ((dskW - 360)/2,300), hangarPic, pset
         line (2,dskH - 75)-(dskW - 2, dskH - 2), rgba(20,20,20,220), bf
         draw string (10, dskH - 64), "CREW HANGARS", 0, starter3
         screenCopy 0, 1
         sleep 800
         '
         SCENE.state    = SCENE._STATE._travelRoom
         djRedundo.fbs_Play_Wave(testWav2,1)
         for i as integer = 0 to 18
            'circle (100, dskH - 220), 22 + i*2 mod 38, rgba(10 + 5*i,100,220,38 + 4*i)
            circle (100 + 40*cos(2*_pi\(i + 1) + _pi), dskH - 220 - 40*sin(2*_pi\(i + 1)) + _pi), 4, rgba(80 + 5*i,100,220,128), , , , f
            flip
            sleep 40
         next i
         sleep 600
      end if
      if summary_QuitBtn._mouseClick then
         put ((dskW - 360)/2,300), quitPic, pset
         line (2,dskH - 75)-(dskW - 2, dskH - 2), rgba(20,20,20,220), bf
         draw string (10, dskH - 64), "QUIT", 0, starter3
         screenCopy 0, 1
         sleep 800
         '
         SCENE.state    = SCENE._STATE._gameExit
         djRedundo.fbs_Play_Wave(testWav2,1)
         for i as integer = 0 to 18
            'circle (100, dskH - 220), 22 + i*2 mod 38, rgba(10 + 5*i,100,220,38 + 4*i)
            circle (100 + 40*cos(2*_pi\(i + 1) + _pi), dskH - 220 - 40*sin(2*_pi\(i + 1)) + _pi), 4, rgba(180 + 2*i,100,120,228), , , , f
            flip
            sleep 40
         next i
         sleep 600
      end if
      
      if (TIMER - summaryStartTime)<1 then
         'if some extra introduction screen is needed...
         scope
            dim as integer imgW, imgH
            imageInfo titlepic, imgW, imgH
            'put ((dskW - imgW)\2, (dskH - imgH)\2), titlepic, TRANS
            'line ((dskW - imgW)\2 + 40, (dskH + imgH)\2 - 16)-step(17*25, 10), rgba(0,200,100, 100), bf
            'line ((dskW - imgW)\2 + 40, (dskH + imgH)\2 - 16)-step(17*25, 10), rgba(100,200,100, 200), b
            'line ((dskW - imgW)\2 + 40, (dskH + imgH)\2 - 16)-step(17*25, 10), rgba(0,200,100, 100), bf
            'line ((dskW - imgW)\2 + 40, (dskH + imgH)\2 - 16)-step(17*25, 10), rgba(100,200,100, 200), b
            'line ((dskW - imgW)\2 + 40, (dskH + imgH)\2 - 16)-step(17*25, 10), rgba(0,140,200, 100), bf
            'line ((dskW - imgW)\2 + 40, (dskH + imgH)\2 - 16)-step(17*25, 10), rgba(100,200,100, 200), b
            for r as integer = 21 to 50
               circle ((dskW)\2 + 60*cos(4*r), (dskH)\2 + 60*sin(4*r)), 8, rgba(100 + 5*r,200,220,128 - r/4), , , , f
               sleep 20
               '
               flip
            next r
         end scope
         
         
         '/
      end if
   screenCopy 0, 1
#endMacro
dim as single     adnMakeUpParam
dim as single     adnMakesUpStartTime  => -1
dim as boolean    adnMakesUp  => FALSE
dim as boolean    adnShowUp   => FALSE

#macro _LABPAGE()   
      'interaction flags___________________________________________________________
      if playAdnButton._mouseClick then
         if not isPlayAdnButtonClicked then  isPlayAdnButtonClicked = TRUE
         if isStopAdnButtonClicked then      isStopAdnButtonClicked = FALSE
      end if
      if stopAdnButton._mouseClick then
         if not isStopAdnButtonClicked then  isStopAdnButtonClicked = TRUE
         if isStopAdnButtonClicked then      isPlayAdnButtonClicked = FALSE
      end if
      'rotations___________________________________________________________________
      if isPlayAdnButtonClicked then
         aRot     = aRot + 1.9
         aRot_2   = aRot_2 - 1
      end if
      if aRot>=360   then aRot   = 0
      if aRot_2<=0   then aRot_2 = 360
      angle       = aRot*TT2._dToR
      angle_2     = aRot_2*TT2._dToR
      cosAngle    = cos(angle)
      sinAngle    = sin(angle)
      cosAngle_2  = cos(angle_2)
      sinAngle_2  = sin(angle_2)
      '3Dpoint rotation
      '3Dpoint rotation <1>
      for i as integer = 0 to (TT2._maxDots - 1)
         x = absolute3DPosition(i)._x
         y = absolute3DPosition(i)._y
         z = absolute3DPosition(i)._z
         'rotation
         relative3DPosition(i)._x = (cosAngle*x) - (sinAngle*z)
         relative3DPosition(i)._y = y
         relative3DPosition(i)._z = (sinAngle*x) + (cosAngle*z)
         relative3DPosition(i)._x = relative3DPosition(i)._x - sp3D._x
         relative3DPosition(i)._y = relative3DPosition(i)._y - sp3D._y
         relative3DPosition(i)._z = relative3DPosition(i)._z - sp3D._z
         relative3DPosition(i)._c = absolute3DPosition(i)._c
         relative3DPosition(i)._id = absolute3DPosition(i)._id
         'refresh the associated 3D coordinates
         geneticUnitInstance(absolute3DPosition(i)._id)._sp3DPtr  = @relative3DPosition(i)
         geneticUnitInstance(absolute3DPosition(i)._id)._id       = relative3DPosition(i)._id
         if activeAdnNodeIndex=absolute3DPosition(i)._id then
            geneticUnitInstance(absolute3DPosition(i)._id)._text  = _
                                                               str(activeAdnNodeIndex)
         else
            geneticUnitInstance(absolute3DPosition(i)._id)._text  = ""
         end if
         geneticUnitInstance(absolute3DPosition(i)._id)._isQuiet = TRUE
      next i
      '3Dpoint rotation <2>
      for i as integer = 0 to (TT2._maxDots - 1)
         x_2 = absolute3DPosition_2(i)._x
         y_2 = absolute3DPosition_2(i)._y
         z_2 = absolute3DPosition_2(i)._z
         'rotation
         relative3DPosition_2(i)._x = y_2
         relative3DPosition_2(i)._y = (cosAngle_2*x_2) - (sinAngle_2*z_2)
         relative3DPosition_2(i)._z = (sinAngle_2*x_2) + (cosAngle_2*z_2)
         relative3DPosition_2(i)._x = relative3DPosition_2(i)._x - sp3D_2._x
         relative3DPosition_2(i)._y = relative3DPosition_2(i)._y - sp3D_2._y
         relative3DPosition_2(i)._z = relative3DPosition_2(i)._z - sp3D_2._z
         relative3DPosition_2(i)._c = absolute3DPosition_2(i)._c
         relative3DPosition_2(i)._id = absolute3DPosition_2(i)._id
         'refresh the associated 3D coordinates
         geneticUnitInstance_2(absolute3DPosition_2(i)._id)._sp3DPtr = @relative3DPosition_2(i)
         geneticUnitInstance_2(absolute3DPosition_2(i)._id)._id      = relative3DPosition_2(i)._id
         if activeAdnNodeIndex=absolute3DPosition_2(i)._id then
            geneticUnitInstance_2(absolute3DPosition_2(i)._id)._text = _
                                                               str(activeAdnNodeIndex)
         else
            geneticUnitInstance_2(absolute3DPosition_2(i)._id)._text = ""
         end if
         geneticUnitInstance_2(absolute3DPosition_2(i)._id)._isQuiet = TRUE
      next i   
      'sort by distance along z axis
      '*********dodisort************
      TT2.Qsort_z(   relative3DPosition(), _
                     lBound(relative3DPosition  ), _
                     uBound(relative3DPosition) )
      TT2.Qsort_z(   relative3DPosition_2(), _
                     lBound(relative3DPosition_2), _
                     uBound(relative3DPosition_2) )

      color 0, 0   
      '_______________________________________________________________________________
      '_______________________________________________________________________________
      screenSet 0, 1
         line (0,40)-(1919,1079), rgba(0,85,155,105), bf
         
         line ((dskW - 390)/2, 280)-step(400,400), rgba(50,250,255, 12), bf
         for i as integer = 0 to 5
            line ((dskW - 390)/2, 280 + i * 24)-step(400,400 - i*24), rgba(50,55,250 - i*4, 12), bf
         next i
         
         if (TIMER - labStartTime)<2 then
            'if some extra introduction screen is needed...
            scope
               dim as integer imgW, imgH
               imageInfo lockedpanpic, imgW, imgH
               put ((TIMER - labStartTime)*90\2, 90), lockedpanpic, PSET
            end scope 
         else
            scope
               dim as integer imgW, imgH
               imageInfo lockedpanpic, imgW, imgH
               put (90, 90), lockedpanpic, PSET
            end scope
         end if
         
         scope
            dim as integer imgW, imgH
            imageInfo labcrewpic, imgW, imgH
            put (40, dskH - imgH*0.88), labcrewpic, TRANS
         end scope
         
         scope
            dim as integer imgW, imgH
            imageInfo modulepic, imgW, imgH
            put (220, 80), modulepic, TRANS
            put (220 + imgW, 80), modulepic, TRANS
            put (220 + 2*imgW, 80), modulepic, TRANS
         end scope          

         rE += 10/TIMER            ''test
         '_________________________________________________________________________
         'ADN viewer animation control button
         playAdnButton.DrawButton()
         stopAdnButton.DrawButton()
         if adnShowUp then ''******************************************************
            'adn<1>
            for i as integer = 0 to TT2._maxDots - 1
               if adnMakesUp then
                  if (TIMER - adnMakesUpStartTime)<2 then
                     adnMakeUpParam = -8000*rnd()
                  else
                     adnMakeUpParam = 0
                     adnMakesUp = FALSE
                  end if
               end if
               
               w = 1 + (relative3DPosition(i)._z/sp3D._z)''this bit adds some perspective
               relative3DPosition(i)._x = (relative3DPosition(i)._x - sp3D._x)/w + sp3D._x
               relative3DPosition(i)._y = (relative3DPosition(i)._y - sp3D._y)/w + sp3D._y
               relative3DPosition(i)._z = (relative3DPosition(i)._z - sp3D._z)/w + sp3D._z + adnMakeUpParam\((TIMER - adnMakesUpStartTime) + 1)            
               
               'convert 3d to 2d coordinates
               px = (relative3DPosition(i)._x/relative3DPosition(i)._z) * 2000
               py = (relative3DPosition(i)._y/relative3DPosition(i)._z) * 2000
               circle (px + dskW/2, py + dskH/2), 8 + 2*rnd(), relative3DPosition(i)._c and rgba(100,50,20 + adnMakeUpParam\((TIMER - adnMakesUpStartTime) + 1),100), , , , f
               circle (px + dskW/2, py + dskH/2), 6, relative3DPosition(i)._c and rgba(100,150,50,220), , , , f
               '
               'refresh dbutton position
               geneticUnitInstance(relative3DPosition(i)._id)._topLeftCornerX   = px + dskW/2 - 2
               geneticUnitInstance(relative3DPosition(i)._id)._topLeftCornerY   = py + dskH/2 - 2
               geneticUnitInstance(relative3DPosition(i)._id)._bottomRightCornerX   = px + dskW/2 + 4
               geneticUnitInstance(relative3DPosition(i)._id)._bottomRightCornerY   = py + dskH/2 + 4
               'draw button
               geneticUnitInstance(relative3DPosition(i)._id).DrawButton()
               '
               'tracking when adn node clicked
               if geneticUnitInstance(relative3DPosition(i)._id)._mouseClick then
                  if not isAdnNodeClicked then isAdnNodeClicked = TRUE
                  activeAdnNodeIndex = relative3DPosition(i)._id
               end if
            next i
            TT2.ShowActiveAdnNodeBox(activeAdnNodeIndex, geneticUnitInstance(), dskW, dskH)
            'adn<2>
            for i as integer = 0 to TT2._maxDots - 1
               if adnMakesUp then
                  if (TIMER - adnMakesUpStartTime)<2 then
                     adnMakeUpParam = -10000 *rnd()
                  else
                     adnMakeUpParam = 0
                     adnMakesUp = FALSE
                  end if
               end if
               
               w = 1 + (relative3DPosition_2(i)._z/sp3D_2._z)
               relative3DPosition_2(i)._x = (relative3DPosition_2(i)._x - sp3D_2._x)/w + sp3D_2._x
               relative3DPosition_2(i)._y = (relative3DPosition_2(i)._y - sp3D_2._y)/w + sp3D_2._y
               relative3DPosition_2(i)._z = (relative3DPosition_2(i)._z - sp3D_2._z)/w + sp3D_2._z + adnMakeUpParam\((TIMER - adnMakesUpStartTime) + 1)
               'convert 3d to 2d coordinates
               px_2 = (relative3DPosition_2(i)._x/relative3DPosition_2(i)._z) * 2000 - dskW\2
               py_2 = (relative3DPosition_2(i)._y/relative3DPosition_2(i)._z) * 2000 + dskH\4
               circle (px_2 + dskW/2, py_2 + dskH/2), 8 + 2*rnd(), relative3DPosition_2(i)._c and rgba(100,50,50+ adnMakeUpParam\((TIMER - adnMakesUpStartTime) + 1),100), , , , f
               circle (px_2 + dskW/2, py_2 + dskH/2), 6, relative3DPosition_2(i)._c and rgba(100,150,50,220), , , , f
               '
               'refresh dbutton position
               geneticUnitInstance_2(relative3DPosition_2(i)._id)._topLeftCornerX   = px_2 + dskW/2 - 2
               geneticUnitInstance_2(relative3DPosition_2(i)._id)._topLeftCornerY   = py_2 + dskH/2 - 2
               geneticUnitInstance_2(relative3DPosition_2(i)._id)._bottomRightCornerX   = px_2 + dskW/2 + 4
               geneticUnitInstance_2(relative3DPosition_2(i)._id)._bottomRightCornerY   = py_2 + dskH/2 + 4
               'draw button
               geneticUnitInstance_2(relative3DPosition_2(i)._id).DrawButton()
               '
               'tracking when adn node clicked
               if geneticUnitInstance_2(relative3DPosition_2(i)._id)._mouseClick then
                  if not isAdnNodeClicked then isAdnNodeClicked = TRUE
                  activeAdnNodeIndex = relative3DPosition_2(i)._id
               end if
            next i
            TT2.ShowActiveAdnNodeBox(activeAdnNodeIndex, geneticUnitInstance_2(), dskW, dskH)
         end if               ''******************************************************
         
         'bottom box
         line (0,dskH - 24)-(dskW - 1,dskH - 1), rgb(0,0,0), bf
         'draw string (4, dskH - 24 + 12), str(ro3Dgd._aA), point(4, dskH - 24 + 12) or rgb(0,90,90)
         draw string (4, dskH - 24 + 12), "node n_" & str(activeAdnNodeIndex) & " selected", point(4, dskH - 24 + 12) or rgb(0,90,90)
         
         
         'search button
         searchButton.DrawButton()     ''test
         if searchButton._mouseClick then
            adnMakesUpStartTime  = TIMER
            adnMakesUp  = TRUE
            adnShowUp   = TRUE
         end if
         
         '_________________________________________________________________________
         
      screenCopy 0, 1
      '_______________________________________________________________________________
'/
#endMacro

#macro _INTERFACEINIT()
   '......array of draggable buttons
   dim as ttDrgDrpBtn.DRAGDROPBUTTON ptr  draggableButtonPtrArray(any)
      'feed the array
      dim as ttDrgDrpBtn.DRAGDROPBUTTON   ddb1
      ddb1._btnShape = ttDrgDrpBtn.DGDPBTN._DGDPBTNSHAPE._naked
      _DYNARRAYINCREASE(draggableButtonPtrArray)
      _ARRAYLASTITEM(draggableButtonPtrArray) = @ddb1
      
      dim as ttDrgDrpBtn.DRAGDROPBUTTON   ddb1b
      ddb1._btnShape = ttDrgDrpBtn.DGDPBTN._DGDPBTNSHAPE._naked
      _DYNARRAYINCREASE(draggableButtonPtrArray)
      _ARRAYLASTITEM(draggableButtonPtrArray) = @ddb1b

      dim as ttDrgDrpBtn.DRAGDROPBUTTON   ddb2
      ddb2._topLeftCorner._y = 800
      ddb2._btnShape = ttDrgDrpBtn.DGDPBTN._DGDPBTNSHAPE._squared
      _DYNARRAYINCREASE(draggableButtonPtrArray)
      _ARRAYLASTITEM(draggableButtonPtrArray) = @ddb2

      dim as ttDrgDrpBtn.DRAGDROPBUTTON   ddb3
      'ddb3._isDragAllowed = FALSE
      ddb3._btnTxt   = "for"
      ddb3._widHei._wid = 8*len(ddb3._btnTxt) + 4
      ddb3._topLeftCorner._y = 100
      ddb3._btnShape = ttDrgDrpBtn.DGDPBTN._DGDPBTNSHAPE._rounded
      _DYNARRAYINCREASE(draggableButtonPtrArray)
      _ARRAYLASTITEM(draggableButtonPtrArray) = @ddb3

   dim as ttDrgDrpBtn.DROPZONE   dz
   dz._dzShape = ttDrgDrpBtn.DZ._DROPZONESHAPE._maximumEllipse

   dim as ttDrgDrpBtn.DROPZONE   dz2
   dz2._dzShape = ttDrgDrpBtn.DZ._DROPZONESHAPE._maximumEllipse
   dz2._topLeftCorner._x = 1000

   ddb1.AddDropZoneToArrayOfPtr(@dz)
   ddb2.AddDropZoneToArrayOfPtr(@dz)
   ddb2.AddDropZoneToArrayOfPtr(@dz2)
   ddb3.AddDropZoneToArrayOfPtr(@dz)

   '......some simple buttons
   dim as integer       summaryStartTime, labStartTime
   dim as ttBtn.DBUTTON   summary_LabBtn => ttBtn.DBUTTON((dskW - 14*18)/2, _
                                                                  100, _
                                                                  14*18, _
                                                                  32, _
                                                                  "GO TO GENETICS LAB", _
                                                                  ttBtn.DBUTTON._BTNBEHAVIOUR._useDelay, _
                                                                  .5)
   with summary_LabBtn
      ._btnColor              => rgba(100,200,000, 200)
      ._btnMouseOverColor     => rgba(000,100,100, 200)
      ._btnMouseClickColor    => rgba(000,100,000, 100)
   end with

   dim as ttBtn.DBUTTON   summary_VesselBtn => ttBtn.DBUTTON((dskW - 14*18)/2, _
                                                                  144, _
                                                                  14*18, _
                                                                  32, _
                                                                  "GET TO CREW HANGAR", _
                                                                  ttBtn.DBUTTON._BTNBEHAVIOUR._useDelay, _
                                                                  .5)
   with summary_VesselBtn
      ._btnColor              => rgba(100,200,000, 200)
      ._btnMouseOverColor     => rgba(000,100,100, 200)
      ._btnMouseClickColor    => rgba(000,100,000, 100)
   end with

   dim as ttBtn.DBUTTON   summary_QuitBtn => ttBtn.DBUTTON((dskW - 14*18)/2, _
                                                                  188, _
                                                                  14*18, _
                                                                  32, _
                                                                  "   QUIT THE GAME", _
                                                                  ttBtn.DBUTTON._BTNBEHAVIOUR._useDelay, _
                                                                  .5)
   with summary_QuitBtn
      ._btnColor              => rgba(200,100,000, 200)
      '._btnMouseOverColor     => rgba(000,100,100, 200)
      '._btnMouseClickColor    => rgba(000,100,000, 100)
   end with
#endMacro

#macro _LABINIT()
   'observation point parameter
   dim as double   rE   => 400.
   dim as double   aE   => 1.4*.275*_pi
   'source plane grid orientation
   dim as double   rA   => 100.
   dim as double   aA   => 225*_pi/180
   dim as single ptr    xForwardPtr => new single

   '(***)
   'interface
   dim as TT2.DBUTTON   playAdnButton
   dim as TT2.DBUTTON   stopAdnButton
   scope
      dim as integer imgW, imgH
      imageInfo biolitpic, imgW, imgH
      dim as integer  topLeftCornerPositionX    => imgW
      dim as integer  topLeftCornerPositionY    => dskH - imgH
      dim as integer  btnWidth                  => 42
      dim as integer  btnHeight                 => 08
      dim as string   buttonText                => "PLAY"
      dim as TT2.DBUTTON._BTNBEHAVIOUR   btnBehav   => TT2.DBUTTON._BTNBEHAVIOUR._useDelay
      dim as double   btnDelay                  => 0.8
      playAdnButton   = _
                     TT2.DBUTTON( topLeftCornerPositionX, _
                                    topLeftCornerPositionY, _
                                    btnWidth, _
                                    btnHeight, _
                                    buttonText, _
                                    btnBehav, _
                                    btnDelay )
   end scope
   scope
      dim as integer imgW, imgH
      imageInfo biolitpic, imgW, imgH
      dim as integer  topLeftCornerPositionX    => imgW
      dim as integer  topLeftCornerPositionY    => dskH - imgH + 32
      dim as integer  btnWidth                  => 42
      dim as integer  btnHeight                 => 08
      dim as string   buttonText                => " STOP"
      dim as TT2.DBUTTON._BTNBEHAVIOUR   btnBehav   => TT2.DBUTTON._BTNBEHAVIOUR._useDelay
      dim as double   btnDelay                  => 0.8
      stopAdnButton   = _
                     TT2.DBUTTON( topLeftCornerPositionX, _
                              topLeftCornerPositionY, _
                              btnWidth, _
                              btnHeight, _
                              buttonText, _
                              btnBehav, _
                              btnDelay )
   end scope
   'entity1
   dim as TT2.SORTABLEP3D    sp3D
      sp3D._x => -0.14*dskW
      sp3D._y => -.12*dskH
      sp3D._z => 1200
   dim as TT2.SORTABLEP3D     absolute3DPosition(TT2._maxDots)
   dim as TT2.SORTABLEP3D     relative3DPosition(TT2._maxDots)
   dim as TT2.GENETICUNIT     geneticUnitInstance(TT2._maxDots)
   dim as double  radius   => 150, _
                  angle1, _
                  angle2
   dim as double  angle, _
                  x, y, z, _
                  rx, ry, rz, _
                  px, py, _
                  cosAngle, sinAngle, aRot
   scope
      dim as integer   index
      for angle1 = 160 to 259 step 4
         for angle2 = 0 to 159 step 80
            absolute3DPosition(index)._x = radius*sin(angle1*TT2._dToR)*cos(angle2*TT2._dToR)
            absolute3DPosition(index)._y = radius*sin(angle1*TT2._dToR)*sin(angle2*TT2._dToR)
            absolute3DPosition(index)._z = radius*cos(angle1*TT2._dToR)
            absolute3DPosition(index)._c = rgb(angle2\2, int(rnd(1)*256), angle1\2)
            absolute3DPosition(index)._id = index
            '
            geneticUnitInstance(index)._sp3DPtr    = @absolute3DPosition(index)
            geneticUnitInstance(index)._id         = index
            geneticUnitInstance(index)._text       = ""
            geneticUnitInstance(index)._isQuiet    = TRUE
            if index<TT2._maxDots then
               index += 1
            end if
         next angle2
      next angle1
   end scope
   'entity2
   dim as TT2.SORTABLEP3D  sp3D_2
      sp3D_2._x => .14*dskW
      sp3D_2._y => -.12*dskH
      sp3D_2._z => 1200
   dim as TT2.SORTABLEP3D   absolute3DPosition_2(TT2._maxDots)
   dim as TT2.SORTABLEP3D   relative3DPosition_2(TT2._maxDots)
   dim as TT2.GENETICUNIT   geneticUnitInstance_2(TT2._maxDots)
   dim as double  radius_2   => 150, _
                  angle1_2, _
                  angle2_2
   dim as double  angle_2, _
                  x_2, y_2, z_2, _
                  rx_2, ry_2, rz_2, _
                  px_2, py_2, _
                  cosAngle_2, sinAngle_2, aRot_2
   scope
      dim as integer   index
      for angle1_2 = 160 to 259 step 4
         for angle2_2 = 0 to 159 step 80
            absolute3DPosition_2(index)._x = radius_2*sin(angle1_2*TT2._dToR)*cos(angle2_2*TT2._dToR)
            absolute3DPosition_2(index)._y = radius_2*sin(angle1_2*TT2._dToR)*sin(angle2_2*TT2._dToR)
            absolute3DPosition_2(index)._z = radius_2*cos(angle1_2*TT2._dToR)
            absolute3DPosition_2(index)._c = absolute3DPosition(index)._c 'rgb(angle2_2\2, int(rnd(1)*256), angle1_2\2)
            absolute3DPosition_2(index)._id = index
            '
            geneticUnitInstance_2(index)._sp3DPtr     = @absolute3DPosition_2(index)
            geneticUnitInstance_2(index)._id          = index
            geneticUnitInstance_2(index)._text        = ""
            geneticUnitInstance_2(index)._isQuiet     = TRUE
            if index<TT2._maxDots then
               index += 1
            end if
         next angle2_2
      next angle1_2
   end scope
   '
   dim as integer   activeAdnNodeIndex       => -1
   dim as boolean   isAdnNodeClicked         => FALSE
   dim as boolean   isPlayAdnButtonClicked   => TRUE
   dim as boolean   isStopAdnButtonClicked   => FALSE
   dim as double w  ''for perspective

   'latest additions...
   'search box
   dim as TT2.DBUTTON   searchButton
   scope
      dim as integer  topLeftCornerPositionX    => dskW\2
      dim as integer  topLeftCornerPositionY    => dskH\2
      dim as integer  btnWidth                  => 42
      dim as integer  btnHeight                 => 08
      dim as string   buttonText                => "GENE"
      dim as TT2.DBUTTON._BTNBEHAVIOUR   btnBehav   => TT2.DBUTTON._BTNBEHAVIOUR._useDelay
      dim as double   btnDelay                  => 0.8
      searchButton   = _
                     TT2.DBUTTON( topLeftCornerPositionX, _
                                    topLeftCornerPositionY, _
                                    btnWidth, _
                                    btnHeight, _
                                    buttonText, _
                                    btnBehav, _
                                    btnDelay )
      with searchButton
         ._btnBorderColor              = rgba(0,100,100,200)  
         ._btnBorderMouseOverColor     = rgba(100,0,100,200)
         ._btnBorderMouseClickColor    = rgba(100,100,0,200)
         ._btnColor                    = rgba(00,0,100,100)
         ._btnMouseOverColor           = rgba(00,100,0,100)
         ._btnMouseClickColor          = rgba(0,100,200,100)
      end with
   end scope
'/
#endMacro


'-------------------------------------------------MAIN--------------------------
'-------------------------------------------------INIT--------------------------
randomize TIMER
'(*)graphics and fonts
'...graphics initialisation
   width 140, 64
   dim as integer dskW, dskH
   screenControl fb.GET_DESKTOP_SIZE, dskW, dskH
   'dskW = 800
   'dskH = 600
   screenRes dskW, dskH, 32, 2, fb.GFX_NO_FRAME + fb.GFX_SHAPED_WINDOW + fb.GFX_ALPHA_PRIMITIVES  
'...fonts initialisation
   dodifont._Createfont    starter  , 1.5 , RGB(100,210,180)   , 20
   dodifont._Createfont    starter2 , 4   , RGB(230,180,110)   , 10
   dodifont._Createfont    starter3 , 1.9 , RGB(190,100,180)   , 2
   dodifont._Createfont    starter4 , 5   , RGB(100,220,080)   , 14
   width dskW\8, dskH\16
'...images loading
   USERACTUATOR.InitImage()
   titlepic       => CreditFBDoc.BMPlOAD(exePath() & "\pic\title.bmp")
   biohazardpic   => CreditFBDoc.BMPlOAD(exePath() & "\pic\biohazard.bmp")
   hangarPic      => CreditFBDoc.BMPlOAD(exePath() & "\pic\hangar.bmp")
   quitPic        => CreditFBDoc.BMPlOAD(exePath() & "\pic\quitStrange.bmp")
   bioPic         => CreditFBDoc.BMPlOAD(exePath() & "\pic\biologist.bmp")
   biolitpic      => CreditFBDoc.BMPlOAD(exePath() & "\pic\biologist_lit.bmp")
   biodarkpic     => CreditFBDoc.BMPlOAD(exePath() & "\pic\biologist_lit_dark.bmp")
   pilotpic       => CreditFBDoc.BMPlOAD(exePath() & "\pic\pilot.bmp")
   labcrewpic     => CreditFBDoc.BMPlOAD(exePath() & "\pic\labcrew_lit.bmp")
   lockedpanpic   => CreditFBDoc.BMPlOAD(exePath() & "\pic\locked_panel.bmp")
   modulepic      => CreditFBDoc.BMPlOAD(exePath() & "\pic\module.bmp")
'...some gui control and settings
'(**)
_PLAY _INTERFACEINIT()
'...adn objects and related settings
'(***)
_PLAY _LABINIT()

'-------------------------------------------------LOOP--------------------------
screenSet 0, 1
color , rgba(255,0,255,255)
cls
do
   'interaction tests-----------------------------------------------------------
   ttRedundo.SCREENTEST.TestScreen()
   if dskW<>ttRedundo.SCREENTEST.scrW orElse dskH<>ttRedundo.SCREENTEST.scrH then
      dskW = ttRedundo.SCREENTEST.scrW
      dskH = ttRedundo.SCREENTEST.scrH
      screenRes dskW, dskH, 32, 2, fb.GFX_NO_FRAME + fb.GFX_SHAPED_WINDOW
      screenSet 0, 1
   end if
   ttredundo.INTERACTIONTEST.TestKeyboard()
   ttredundo.INTERACTIONTEST.TestMouse()
   
   'scenery---------------------------------------------------------------------
   select case SCENE.state
      case SCENE._STATE._gameStart
         _PLAY _GAMESTARTPAGE()
      case SCENE._STATE._summary
         _PLAY _SUMMARYPAGE()
      case SCENE._STATE._laboratory
         _PLAY _LABPAGE()
      case SCENE._STATE._travelRoom
         
      case SCENE._STATE._groundScenery
      
      case SCENE._STATE._endOfGame
         
      case SCENE._STATE._gameExit
         screenSet 0, 1
            draw string (10, dskH - 38), "GAME ENDED", 0, starter3
            screenCopy 0, 1
            exit do
            circle (dskW - 32, +18), 10, rgba(200,0,0, 120), , , , f
         screenCopy 0, 1
   end select

   'loop termination test-------------------------------------------------------
   screenSet 0, 1
      if (ttredundo.INTERACTIONTEST.gmBtn=1) andAlso _ 
         sqr((ttRedundo.INTERACTIONTEST.gmx - (dskW - 32))^2 + (ttRedundo.INTERACTIONTEST.gmy - (+ 20))^2)<10 then
         line (2,dskH - 75)-(dskW - 2, dskH - 2), rgba(20,20,20,220), bf
         draw string (10, dskH - 64), "GAME ENDED", 0, starter2
         circle (dskW - 32, +18), 10, rgba(200,0,200, 120), , , , f
         screenCopy 0, 1
         exit do
      end if
      circle (dskW - 32, +18), 10, rgba(200,0,0, 120), , , , f
   screenCopy 0, 1
   '
   if SCENE.state<>SCENE._STATE._gameStart then
      if (TIMER - summaryStartTime)>1 then
         USERACTUATOR.ShowActuator(USERACTUATOR.mouse_free, 40)
         USERACTUATOR.ShowActuator(USERACTUATOR.mouse_free, 80)
         if ttredundo.INTERACTIONTEST.gmBtn>0 then USERACTUATOR.ShowActuator(USERACTUATOR.mouse_bip, 40)
         if ttredundo.INTERACTIONTEST.scanCode = fb.SC_UP      then USERACTUATOR.ShowActuator(USERACTUATOR.up_arrow, 80)
         if ttredundo.INTERACTIONTEST.scanCode = fb.SC_DOWN    then USERACTUATOR.ShowActuator(USERACTUATOR.down_arrow, 80)
         if ttredundo.INTERACTIONTEST.scanCode = fb.SC_LEFT    then USERACTUATOR.ShowActuator(USERACTUATOR.left_arrow, 80)
         if ttredundo.INTERACTIONTEST.scanCode = fb.SC_RIGHT   then USERACTUATOR.ShowActuator(USERACTUATOR.right_arrow, 80)
         flip
      end if
   end if
   '
   sleep 15
   _SLEEP1MS()
loop until inkey()=chr(27)


'-------------------------------------------------CLEA--------------------------

'-------------------------------------------------END.--------------------------
getKey()


'(eof)