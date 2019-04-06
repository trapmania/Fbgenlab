''A LOVE LETTER TO FREEBASIC :: FB GENETICS LABORATORY GAME V1.0
''written in FREEBASIC by TOURIST TRAP in March 2019
''warning --> won't compile to Windows in 64bits for now

''---------------------------------------------------------------note 17/03/2019
''>my new alternative entry for the fb community game contest by Lachie (my first try was merely aborted)
''>lots of stuff here come right from the fb official forum and its very kindy contributors (duely credited along the program)
''>other stuff, and the idea of this thing is from me I think, probably inspired from things I can't remember where they came from first
''>about my part anyway, I simply brought back old materials
'' where namespaces proved fine for welding successfully the variety of pieces of code
''>above all, I hope the program will work, 
'' because this is still such a challenge for the peasant !
''>thanks go to Lachie for impulsing this, and for the fb community for its talent and for the help on demand
''------------------------------------------------------------------------------


#include "fbgfx.bi"
#include once "incs/dodicat_fonts.bas"

dim shared as fb.IMAGE ptr    titlepic, biohazardpic, hangarPic, quitPic
dim shared as any ptr         starter, starter2, starter3, starter4


'Credit::FBCommunity
#ifDef __FB_WIN32__
#print "compiling to .. a win32 API platform"
   #include once "incs/landeelDpiAwarerenessSetter.bas"
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
	Const NULL As Any Ptr = 0
	Function BMPlOAD( ByRef filename As Const String ) As Any Ptr
	    Dim As Long filenum, bmpwidth, bmpheight
	    Dim As Any Ptr img
	    '' open BMP file
	    filenum = FreeFile()
	    If Open( filename For Binary Access Read As #filenum ) <> 0 Then Return NULL
	        '' retrieve BMP dimensions
	        Get #filenum, 19, bmpwidth
	        Get #filenum, 23, bmpheight
	    Close #filenum
	    '' create image with BMP dimensions
	    img = ImageCreate( bmpwidth, Abs(bmpheight) )
	    If img = NULL Then Return NULL
	    '' load BMP file into image buffer
	    If BLoad( filename, img )<>0 Then ImageDestroy( img ): Return NULL
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

''note:  there may be redundancy in this program due to the lack of time endured
''       hopefully this shouldn't really be what will hurt at the execution time
#if __FB_64BIT__
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
end nameSpace

nameSpace dafhiRedundo
   '--------------------------------------
   'a dahfi's IMAGEVARS refactored version
   '--------------------------------------
   'list of UDTs:
   '   V3
   '   P3D
   '   AXIS3D
   '   IMAGER
   '   AADOT
   '   SCRPOS
   '   WIDHEI
   '   BOX
   '   MONOAPPSCREEN
   type V3
      as single   _x, _y, _z
      as single   _radius
      as long     _color
   end type
   type P3D   as V3

   type AXIS3D
      as V3       _axisX = (1,0,0)
      as V3       _axisY = (0,-1,0)
      as V3       _axisZ = (0,0,1)
      as single   _x, _y, _z
   end type

   type IMAGER
      declare constructor()
      enum _CREATIONMODE
         _asScreen   = 0
         _asImage    = 1
      end enum
      declare constructor(byval CreationMode as _CREATIONMODE)
      declare destructor()
      private:
      declare sub VarPoolInit()
      declare sub ImgDestroy()
      public:
      declare sub ClrScr(byval Colour as ulong=&HFF000000)
      declare sub ScrInf()
      declare sub InitAsScreen(  byval Wid as single=-1, _
                                 byval Hei as single=-1, _
                                 byval Bpp as uinteger=32, _
                                 byval NumPage as integer=2, _
                                 byval Flag as integer=0 )
      declare function CreateAsImage(  byval Wid as long=-1, _
                                       byval Hei as long=0, _
                                       byval Colour as ulong=rgb(0,0,0) ) _
                                       as any ptr
      declare sub CheckerCreate( byval PColor as ulong=rgba(145,145,145,255), _
                                 byval Size   as uinteger=12   )
      as any ptr  _img, _pix
      as integer  _w, _h, _
                  _bpp, _bypp, _pitch, _
                  _numpage, _flag, _rate, _
                  _pitchBy, _wm, _hm
      as boolean  _isScreen
      as single   _midx, _midy, _
                  _midxm, _midym, _diag
      as string   _driverName
   end type
   constructor IMAGER()
      screen 0
      dim as integer   deskW, deskH
      screenInfo deskW, deskH
      THIS.InitAsScreen(deskW, deskH, 32, 2, fb.GFX_NO_FRAME + fb.GFX_SHAPED_WINDOW)
      ClrScr(rgb(255,0,255))
   end constructor
   constructor IMAGER(byval CreationMode as _CREATIONMODE)
      select case CreationMode
         case IMAGER._CREATIONMODE._asScreen
            screen 0
            dim as integer   deskW, deskH
            screenInfo deskW, deskH
            THIS.InitAsScreen(deskW, deskH, 32, 2, fb.GFX_NO_FRAME + fb.GFX_SHAPED_WINDOW + fb.GFX_ALPHA_PRIMITIVES + fb.GFX_HIGH_PRIORITY)
            ClrScr(rgb(255,0,255))
         case IMAGER._CREATIONMODE._asImage
            '
      end select
   end constructor
   destructor IMAGER()
       ImgDestroy()
   end destructor
   sub IMAGER.VarPoolInit()
      with THIS
         ._wm     => ._w - 1  : ._midx   => ._w/2
         ._midxm  => ._wm/2   : ._hm     => ._h - 1
         ._midy   => ._h/2    : ._midym  => ._hm/2
      end with
      '
      THIS._diag => sqr(THIS._w*THIS._w + THIS._h*THIS._h)
      '
      if THIS._bypp<>0 then
         THIS._pitchBy = THIS._pitch\THIS._bypp
      end if
   end sub
   sub IMAGER.ImgDestroy()
      if THIS._img<>0 then
         imageDestroy(THIS._img)
         THIS._img = 0
      end if
   end sub
   sub IMAGER.ClrScr(byval Colour as ulong=&HFF000000)
      line (0,0)-(THIS._wm, THIS._hm), Colour, bf
   end sub
   sub IMAGER.ScrInf()
      with THIS
         screenInfo  ._w, ._h, ._bpp, _
                     ._bypp, ._pitch, ._rate, _
                     ._driverName
         ._pix => screenPtr()
      end with
      '
      THIS.VarPoolInit()
   end sub
   sub IMAGER.InitAsScreen(   byval Wid      as single=-1, _
                              byval Hei      as single=-1, _
                              byval Bpp      as uinteger=32, _
                              byval NumPage  as integer=2, _
                              byval Flag     as integer=0   )
      THIS.ImgDestroy()
      dim as integer ww, hh
      screenInfo     ww, hh
      '
      Wid   = abs(Wid)
      if Wid<=1 then
         Wid *= ww
      end if
      Hei   = abs(Hei)
      if Hei<=1 then
         Hei *=  hh
      end if
      '
      with THIS
         ._w         =>    Wid
         ._h         =>    Hei
         ._Bpp       =>    Bpp
         ._flag      or=>  8
         ._numpage   =>    Numpage
         ._flag      =>    Flag
         ._isScreen  =>    TRUE
      end with
      '
      screenRes   Wid, _
                  Hei, _
                  Bpp, _
                  Numpage, _
                  Flag
      THIS.ScrInf()
      '
      if NumPage>1 then
         screenSet 0, 1
      end if
   end sub
   function IMAGER.CreateAsImage(   byval Wid as long=-1, _
                                    byval Hei as long=0, _
                                    byval Colour as ulong=rgb(0,0,0) ) _
                                    as any ptr
      THIS.ImgDestroy()
      '
      if Hei=0 then
         THIS.ScrInf()
         Wid = THIS._w
         Hei = THIS._h
      end if
      '
      THIS._img => imageCreate( Wid, Hei, (rgba(8,20,20,199) * rgba(0,0,65,205)), 32 ) ''colour
      line  THIS._img, (0,0)-(Wid - 1, 40), rgba(40,14,18,180), bf
      imageInfo   THIS._img, _
                  THIS._w, _
                  THIS._h, _
                  THIS._bypp, _
                  THIS._pitch, _
                  THIS._pix
      THIS._bpp = THIS._bypp*8
      '
      THIS.VarPoolInit()
      THIS._isScreen => FALSE
      '
      return THIS._img
   end function
   sub IMAGER.CheckerCreate(byval PColor as ulong, byval Size as uinteger)
      dim as uinteger sizeDouble => Size*2
      dim as uinteger sizeM      => Size - 1
      '
      for y as integer = 40 to THIS._hm step sizeDouble
         for x as integer = -Size*( (y/sizeDouble)=int(y/sizeDouble) ) + 1 to _
                                                            THIS._wm    step _
                                                            sizeDouble
            line  THIS._img, _
                  (x, (y*rnd() + 1) * y)-step(-x*sizeM + 100*rnd(), y*sizeM*2), _
                  (x*TIMER - y*PColor) and rgba(225*rnd() + 50,80 * rnd(),100, 50), _
                  b, _ 
                  &b0000000001
         next x
      next y
   end sub

   #macro _ALPHA256(ret, back, fore, am, a256)
     ret=((_
             ( fore and &hFF00FF)*a256 + _
             ( back and &hFF00FF)*am + &h800080 ) and &hFF00FF00 or _
             (_
                 ( fore and &H00FF00)*a256 + _
                 ( back and &h00FF00)*am + &h008000 ) and &h00FF0000) shr 8
   #endMacro
   type AADOT
      declare constructor()
      declare sub RenderTarget(byval Pti as IMAGER ptr)
      declare sub DrawAadot(  byval X as single=0, _
                              byval Y as single=0, _
                              byval C as ulong=&hFFFFFFFF   )
         as single      _radius
         as single      _alpha
         as boolean     _isOutlined
         as IMAGER ptr  _imagerPtr
         as any ptr     _pixPtr
      private:
         as single      _slope
         as single      _slope_X2
   end type
   constructor AADOT()
      with THIS
         ._radius       => 10.65
         ._alpha        => 1.
         ._isOutlined   => FALSE
      end with
   end constructor
   sub AADOT.RenderTarget(byval Pti as IMAGER ptr)
      if pti->_isScreen then
         THIS._pixPtr = screenPtr()
      else
         THIS._pixPtr = Pti->_pix
      end if
      '
      THIS._imagerPtr = Pti
   end sub
   sub AADOT.DrawAadot( byval X as single=0, _
                        byval Y as single=0, _
                        byval C as ulong=&hFFFFFFFF   )
      if THIS._imagerPtr->_h<1 then
         exit sub
      end if
      '
      THIS._slope       = THIS._alpha*256
      THIS._slope_X2    = THIS._slope*2
      '
      dim as single     coneHei     => THIS._radius*THIS._slope
      dim as integer    x1          => X - THIS._radius
      dim as integer    x2          => X + THIS._radius
      x1 += x1*( x1<0 )
      dim as integer    y1          => Y - THIS._radius
      dim as integer    y2          => Y + THIS._radius
      y1 += y1*( y1<0 )
      dim as single     dxClip      => THIS._slope*(X - x1)
      dim as single     dy          => THIS._slope*(Y - y1)
      x2 += (x2 - THIS._imagerPtr->_wm)*(x2>THIS._imagerPtr->_wm)
      y2 += (y2 - THIS._imagerPtr->_hm)*(y2>THIS._imagerPtr->_hm)
      dim as integer    pWidm       => x2 - x1
      dim as ulong ptr  pCorner     => THIS._pixPtr
      pCorner += x1 + y1*THIS._imagerPtr->_pitchBy
      '
      if THIS._isOutlined then
        for py as ulong ptr = pCorner                                         to _
                              @pCorner[ (y2 - y1)*THIS._imagerPtr->_pitchBy ] step _
                              THIS._imagerPtr->_pitchBy
            '
            dim as single ySq    => dy*dy
            dim as single dx     => dxClip
            for px as ulong ptr = py to py + pWidm
               '
               dim as integer alph  => coneHei - sqr(dx*dx + ySq)
               if alph>THIS._slope then alph = THIS._slope_X2 - alph
               if alph>0 then
                  dim as integer  alphM => 256 - alph
                  _ALPHA256(  *px, _
                              *px, _
                              C, _
                              alphM, _
                              alph  )
               end if
               dx -= THIS._slope
               '
            next px
            dy -= THIS._slope
            '
        next py
      else
         for py as ulong ptr = pCorner                                  to _
                        @pCorner[ (y2 - y1)*THIS._imagerPtr->_pitchBy ] step _
                        THIS._imagerPtr->_pitchBy
            '
            dim as single ySq => dy*dy
            dim as single dx  => dxClip
            for px as ulong ptr = py to py + pWidm
               dim as integer  alph => coneHei - sqr(dx*dx + ySq)
               '
               if alph> THIS._slope then
                  alph = THIS._slope
               else
                  alph += alph*(alph<0)
               end if
               '
               dim as integer  alphM => 256 - alph
               _ALPHA256(  *px, _
                           *px, _
                           C, _
                           alphM, _
                           alph  )
               dx -= THIS._slope
            next px
            dy -= THIS._slope
         next py
      end if
   end sub

   type SCRPOS
         as integer   _scrPosX
         as integer   _scrPosY
   end Type

   type WIDHEI
      as integer  _wid
      as integer  _hei
   end type

   type BOX
      declare property Xi() as integer
      declare property Xi(byval as integer)
      declare property Yi() as integer
      declare property Yi(byval as integer)
      declare property W() as integer
      declare property W(byval as integer)
      declare property H() as integer
      declare property H(byval as integer)
      declare property Xf() as integer
      declare property Xf(byval as integer)
      declare property Yf() as integer
      declare property Yf(byval as integer)
         as SCRPOS   _scrPos
         as WIDHEI   _widHei
   end type
   property BOX.Xi() as integer
      return THIS._scrPos._scrPosX
   end property
   property BOX.Xi(byval SetValue as integer)
      THIS._scrPos._scrPosX = SetValue
   end property
   property BOX.Yi() as integer
      return THIS._scrPos._scrPosY
   End Property
   property BOX.Yi(byval SetValue as integer)
      THIS._scrPos._scrPosY = SetValue
   end property
   property BOX.W() as integer
      return THIS._widHei._wid
   end property
   property BOX.W(byval SetValue as integer)
      THIS._widHei._wid = SetValue
   End Property
   property BOX.H() as integer
      return THIS._widHei._hei
   end property
   property BOX.H(byval SetValue as integer)
      THIS._widHei._hei = SetValue
   end property
   property BOX.Xf() as integer
      return ( THIS._scrPos._scrPosX + THIS._widHei._wid - 1 )
   end property
   property BOX.Xf(byval SetValue as integer)
      THIS._widHei._wid = SetValue - THIS._scrPos._scrPosX + 1
   end property
   property BOX.Yf() as integer
      return ( THIS._scrPos._scrPosY + THIS._widHei._hei - 1 )
   end property
   property BOX.Yf(byval SetValue as integer)
      THIS._widHei._hei = SetValue - THIS._scrPos._scrPosY + 1
   end property

   type ARRAYOFP3D
      declare constructor()
      declare constructor( P3DArray() as P3D )
         as P3D   _arrayOfP3D(any)
   end type
   constructor ARRAYOFP3D()
      '
   end constructor
   constructor ARRAYOFP3D( P3DArray() as P3D )
      redim THIS._arrayOfP3D(uBound(P3DArray) - lBound(P3DArray))
      for index as integer = lBound(P3DArray) to uBound(P3DArray)
         THIS._arrayOfP3D(index - lBound(P3DArray)) = P3DArray(index)
      next index
   end constructor

   type MONOAPPSCREEN
      declare constructor()
      declare constructor( byval ImgWid as integer, _
                           byval ImgHei as integer )
      declare constructor( byval ScreenBuffer as IMAGER, _
                           byval BackgroundImg as IMAGER, _
                           byval ZoomFactor as single, _
                           byval MainAxis as AXIS3D, _
                           ArrayOfPoint() as P3D   )
      declare property BckgndImgXi() as integer
      declare property BckgndImgXi(byval as integer)
      declare property BckgndImgYi() as integer
      declare property BckgndImgYi(byval as integer)
      declare property BckgndImgXf() as integer
      declare property BckgndImgYf() as integer
      declare property DeltaXFromCenter() as integer
      declare property DeltaYFromCenter() as integer
      declare sub PlugArrayOfP3D( P3DArray() as P3D )
      declare sub UnPlugAllArrayOfP3D()
      declare sub QsortZ( Array() As P3D, Begin as long, Finish as long)
      declare sub TopBarInit()
      declare sub BuildArrayOfBorderLinePoint()
      declare sub BuildArrayOfTopBarFilledBoxPoint()
      declare sub BuildFullArrayOfPoint()
      declare function MouseIsInTopBar(   byval GmX as integer, _
                                          byval GmY as integer ) _
                                       as boolean
      declare sub TestMouse()
      declare sub DrawHelper()
      declare sub DrawTopBar()
      declare sub RenderAppScreenFramework()
      declare sub RenderAppScreenContent()
         as IMAGER      _screenBuffer
         as IMAGER      _mainBackgroundImageBuffer
         as integer     _mainBackgroundImageBufferTopLeftCornerX
         as integer     _mainBackgroundImageBufferTopLeftCornerY
         as P3D         _arrayOfBorderLinePoint(any)
         as P3D         _arrayOfTopBarFilledBoxPoint(any)
         as P3D         _arrayOfPoint(any)
         '
         as ARRAYOFP3D   _arrayOfArrayOfP3D(any)
         '
         as single      _zoomFactor
         as AXIS3D      _mainAxis
         '
         as BOX         _topBar
         as boolean     _hasTopBarMouseOver
         as boolean     _hasTopBarMouseClick
         as boolean     _hasTopBarDragStarted
         as boolean     _hasMovedAtLeastOnce
         as integer     _mouseXatDragStart
         as integer     _mouseYatDragStart
         '
         as ulong       _borderColor
         as boolean     _hasMouseOver
         as boolean     _hasMouseClick
   end type
   type as MONOAPPSCREEN   MAPPS
   constructor MONOAPPSCREEN()
      with THIS
         ._screenBuffer                => IMAGER(IMAGER._CREATIONMODE._asScreen)
         ._mainBackgroundImageBuffer   => IMAGER(IMAGER._CREATIONMODE._asImage)
         ._zoomFactor                  => 1
      end with
      '
      with THIS._mainBackgroundImageBuffer
         .CreateAsImage THIS._screenBuffer._w\4, _
                        THIS._screenBuffer._h\4, _
                        rgb(48,48,48)
         .CheckerCreate rgb(92,92,92), 30
      end with
      '
      THIS.BckgndImgXi  => (THIS._screenBuffer._w - THIS._mainBackgroundImageBuffer._w)\2
      THIS.BckgndImgYi  => (THIS._screenBuffer._h - THIS._mainBackgroundImageBuffer._h)\2
      redim preserve THIS._arrayOfPoint(0)
      '
      THIS.TopBarInit()
      THIS.BuildArrayOfBorderLinePoint()
      THIS.BuildArrayOfTopBarFilledBoxPoint()
      THIS.BuildFullArrayOfPoint()
   end constructor
   constructor MONOAPPSCREEN( byval ImgWid as integer, _
                              byval ImgHei as integer )
      with THIS
         ._screenBuffer                => IMAGER(IMAGER._CREATIONMODE._asScreen, )
         ._mainBackgroundImageBuffer   => IMAGER(IMAGER._CREATIONMODE._asImage)
         ._zoomFactor                  => 1
      end with
      '
      with THIS._mainBackgroundImageBuffer
         .CreateAsImage ImgWid, _
                        ImgHei, _
                        rgb(0,110,165)
         .CheckerCreate rgb(40,40,10), 2080
      end with
      '
      THIS.BckgndImgXi  => (THIS._screenBuffer._w - THIS._mainBackgroundImageBuffer._w)\2
      THIS.BckgndImgYi  => (THIS._screenBuffer._h - THIS._mainBackgroundImageBuffer._h)\2
      redim preserve THIS._arrayOfPoint(0)
      '
      THIS.TopBarInit()
      THIS.BuildArrayOfBorderLinePoint()
      THIS.BuildArrayOfTopBarFilledBoxPoint()
      THIS.BuildFullArrayOfPoint()
   end constructor
   constructor MONOAPPSCREEN( byval ScreenBuffer as IMAGER, _
                              byval BackgroundImg as IMAGER, _
                              byval ZoomFactor as single, _
                              byval MainAxis as AXIS3D, _
                              ArrayOfPoint() as P3D   )
      with THIS
         ._screenBuffer                => ScreenBuffer
         ._mainBackgroundImageBuffer   => BackgroundImg
         ._mainAxis                    => MainAxis
         ._zoomFactor                  => ZoomFactor
      end with
      '
      redim THIS._arrayOfPoint(uBound(ArrayOfPoint) - lBound(ArrayOfPoint))
      for index as integer = lBound(ArrayOfPoint) to uBound(ArrayOfPoint)
         THIS._arrayOfPoint(index - lBound(ArrayOfPoint)) => ArrayOfPoint(index)
      next index
      '
      THIS.BckgndImgXi  => (THIS._screenBuffer._w - THIS._mainBackgroundImageBuffer._w)\2
      THIS.BckgndImgYi  => (THIS._screenBuffer._h - THIS._mainBackgroundImageBuffer._h)\2
      '
      THIS.TopBarInit()
      THIS.BuildArrayOfBorderLinePoint()
      THIS.BuildArrayOfTopBarFilledBoxPoint()
      THIS.BuildFullArrayOfPoint()
   end constructor
   property MAPPS.BckgndImgXi() as integer
      return THIS._mainBackgroundImageBufferTopLeftCornerX
   end property
   property MAPPS.BckgndImgXi(byval SetValue as integer)
      THIS._mainBackgroundImageBufferTopLeftCornerX = SetValue
   end property
   property MAPPS.BckgndImgYi() as integer
      return THIS._mainBackgroundImageBufferTopLeftCornerY
   end property
   property MAPPS.BckgndImgYi(byval SetValue as integer)
      THIS._mainBackgroundImageBufferTopLeftCornerY = SetValue
   end property
   property MAPPS.BckgndImgXf() as integer
      return ( THIS._mainBackgroundImageBufferTopLeftCornerX + THIS._mainBackgroundImageBuffer._w - 1)
   end property
   property MAPPS.BckgndImgYf() as integer
      return ( THIS._mainBackgroundImageBufferTopLeftCornerY + THIS._mainBackgroundImageBuffer._h - 1)
   end property
   property MAPPS.DeltaXFromCenter() as integer
      dim as integer   initialX = (THIS._screenBuffer._w - THIS._mainBackgroundImageBuffer._w)\2
      '
      return (THIS.BckgndImgXi - initialX)/5
   end property
   property MAPPS.DeltaYFromCenter() as integer
      dim as integer   initialY = (THIS._screenBuffer._h - THIS._mainBackgroundImageBuffer._h)\2
      '
      return (THIS.BckgndImgYi - initialY)/5
   end property
   sub MAPPS.PlugArrayOfP3D( P3DArray() as P3D )
      THIS.BuildArrayOfBorderLinePoint()
      THIS.BuildArrayOfTopBarFilledBoxPoint()
      THIS.BuildFullArrayOfPoint()
      '
      var initialBound => uBound(THIS._arrayOfPoint)
      redim preserve THIS._arrayOfPoint(uBound(THIS._arrayOfPoint) + uBound(P3DArray))
      '
      for index as integer = 0 to uBound(P3DArray)
         THIS._arrayOfPoint(initialBound + index) = P3DArray(index)
      next index
   end sub
   sub MAPPS.UnPlugAllArrayOfP3D()
      erase THIS._arrayOfPoint
      '
      THIS.BuildArrayOfBorderLinePoint()
      THIS.BuildArrayOfTopBarFilledBoxPoint()
      THIS.BuildFullArrayOfPoint()
   end sub
   sub MAPPS.QsortZ( Array() As P3D, Begin as long, Finish as long)
      'author::dodicat@fb.net
      dim as long      i => Begin
      dim as long      j => Finish
      '
      dim as P3D      item => Array( ( (i + j)\2 ) )
      '
      while i<=j
         while Array(i)._z>item._z   : i += 1   : wend
         while Array(j)._z<item._z   : j -= 1   : wend
         if i<j then
            swap Array(i), Array(j)
            i += 1
            j -= 1
         elseIf i=j then
            i += 1
            j -= 1
         end if
      wend
      '
      if j>Begin then QsortZ( array() , Begin, j )
      if i<Finish then QsortZ( array(), i, Finish)
   end sub
   sub MAPPS.TopBarInit()
      THIS._topBar._scrPos._scrPosX => THIS.BckgndImgXi
      THIS._topBar._scrPos._scrPosY => THIS.BckgndImgYi
      THIS._topBar._widHei._wid     => THIS._mainBackgroundImageBuffer._w
      THIS._topBar._widHei._hei     => 24
   end sub
   sub MAPPS.BuildArrayOfBorderLinePoint()
      dim as integer   horizontalSemiWidth   => (THIS._mainBackgroundImageBuffer._w)\10
      dim as integer   verticalSemiHeight    => (THIS._mainBackgroundImageBuffer._h)\10
      redim THIS._arrayOfBorderLinePoint( 4*( horizontalSemiWidth + verticalSemiHeight ) + 1 )
      '
      dim as integer   index
      dim as integer   indexOffset
      'left vertical side
      indexOffset += 0
      for index = indexOffset to indexOffset + 2*verticalSemiHeight + 1
         'define point at index
         THIS._arrayOfBorderLinePoint(index)._x       = -horizontalSemiWidth + THIS.DeltaXFromCenter
         THIS._arrayOfBorderLinePoint(index)._z       = 0.1
         THIS._arrayOfBorderLinePoint(index)._radius  = 10
         THIS._arrayOfBorderLinePoint(index)._color   = rgb(200,0,0)
         if index<=verticalSemiHeight then
            THIS._arrayOfBorderLinePoint(index)._y = index - indexOffset + THIS.DeltaYFromCenter
         else
            THIS._arrayOfBorderLinePoint(index)._y = verticalSemiHeight - (index - indexOffset) + THIS.DeltaYFromCenter
         end if
      next index
      'right vertical side
      indexOffset += 2*verticalSemiHeight + 1
      for index = indexOffset to indexOffset + 2*verticalSemiHeight
         'define point at index
         THIS._arrayOfBorderLinePoint(index)._x       = +horizontalSemiWidth + THIS.DeltaXFromCenter - 1
         THIS._arrayOfBorderLinePoint(index)._z       = 0.1
         THIS._arrayOfBorderLinePoint(index)._radius  = 10
         THIS._arrayOfBorderLinePoint(index)._color   = rgb(200,0,0)
         if index<=verticalSemiHeight then
            THIS._arrayOfBorderLinePoint(index)._y = index - indexOffset + THIS.DeltaYFromCenter
         else
            THIS._arrayOfBorderLinePoint(index)._y = verticalSemiHeight - (index - indexOffset) + THIS.DeltaYFromCenter
         end if
      next index
      'bottom vertical side
      indexOffset += 2*verticalSemiHeight
      for index = indexOffset to indexOffset + 0*2*horizontalSemiWidth
         'define point at index
         THIS._arrayOfBorderLinePoint(index)._y       = +verticalSemiHeight + THIS.DeltaYFromCenter
         THIS._arrayOfBorderLinePoint(index)._z       = 0.1
         THIS._arrayOfBorderLinePoint(index)._radius  = 10
         THIS._arrayOfBorderLinePoint(index)._color   = rgb(200,0,0)
         if index<=verticalSemiHeight then
            THIS._arrayOfBorderLinePoint(index)._x = index - indexOffset + THIS.DeltaXFromCenter - 1
         else
            THIS._arrayOfBorderLinePoint(index)._x = horizontalSemiWidth - (index - indexOffset) + THIS.DeltaXFromCenter - 1
         end if
      next index
      'top vertical side
      indexOffset += 2*horizontalSemiWidth
      ''note: exit button
      for index = indexOffset to indexOffset + 0*2*horizontalSemiWidth
         'define point at index
         THIS._arrayOfBorderLinePoint(index)._y       = -verticalSemiHeight + THIS.DeltaYFromCenter
         THIS._arrayOfBorderLinePoint(index)._z       = 0.1
         THIS._arrayOfBorderLinePoint(index)._radius  = 160
         THIS._arrayOfBorderLinePoint(index)._color   = rgb(200,080,080)
         if index<=verticalSemiHeight then
            THIS._arrayOfBorderLinePoint(index)._x = index - indexOffset + THIS.DeltaXFromCenter - 1
         else
            THIS._arrayOfBorderLinePoint(index)._x = horizontalSemiWidth - (index - indexOffset) + THIS.DeltaXFromCenter - 1
         end if
      next index
   end sub
   sub MAPPS.BuildArrayOfTopBarFilledBoxPoint()
      dim as integer   horizontalSemiWidth   => (THIS._mainBackgroundImageBuffer._w)\10
      dim as integer   verticalSemiHeight    => (THIS._mainBackgroundImageBuffer._h)\10
      dim as integer   totalArrayCount       => (  2*(horizontalSemiWidth - 2) + 1)*(THIS._topBar._widHei._hei\4 - 1   )
      redim THIS._arrayOfTopBarFilledBoxPoint(24*horizontalSemiWidth)
      '
      dim as integer   x, y
      for x = -(horizontalSemiWidth - 2) to horizontalSemiWidth - 2
         for y = 2 to THIS._topBar._widHei._hei\4 step 1
            THIS._arrayOfTopBarFilledBoxPoint( _
            (x - (horizontalSemiWidth - 2)) + 2*y*horizontalSemiWidth   )._x  = x + THIS.DeltaXFromCenter
            THIS._arrayOfTopBarFilledBoxPoint( _
            (x - (horizontalSemiWidth - 2)) + 2*y*horizontalSemiWidth   )._y  = -verticalSemiHeight + y + THIS.DeltaYFromCenter
            THIS._arrayOfTopBarFilledBoxPoint( _
            (x - (horizontalSemiWidth - 2)) + 2*y*horizontalSemiWidth   )._z  = 0.1
            THIS._arrayOfTopBarFilledBoxPoint( _
            (x - (horizontalSemiWidth - 2)) + 2*y*horizontalSemiWidth   )._radius   = 10
            THIS._arrayOfTopBarFilledBoxPoint( _
            (x - (horizontalSemiWidth - 2)) + 2*y*horizontalSemiWidth   )._color    = rgb(40,130,100)
         next y
      next x
   end sub
   sub MAPPS.BuildFullArrayOfPoint()
      dim as integer   totalPoint
      totalPoint =   uBound(THIS._arrayOfBorderLinePoint) + _ 
                     uBound(THIS._arrayOfTopBarFilledBoxPoint)
      '
      redim THIS._arrayOfPoint(totalPoint)
      for index as integer = lBound(THIS._arrayOfBorderLinePoint) to _
                                                uBound(THIS._arrayOfBorderLinePoint)
         THIS._arrayOfPoint(index + lBound(THIS._arrayOfBorderLinePoint)) = _
                                                THIS._arrayOfBorderLinePoint(index)
      next index
      for index as integer = lBound(THIS._arrayOfTopBarFilledBoxPoint) to _
                                                uBound(THIS._arrayOfTopBarFilledBoxPoint)
         THIS._arrayOfPoint(  index                                     + _
                              lBound(THIS._arrayOfTopBarFilledBoxPoint) + _
                              uBound(THIS._arrayOfBorderLinePoint)   )  = _
                                                THIS._arrayOfTopBarFilledBoxPoint(index)
      next index
   end sub
   function MAPPS.MouseIsInTopBar(  byval GmX as integer, _
                                    byval GmY as integer ) as boolean
      if GmX>=THIS._topBar.Xi      andAlso _
         GmX<=THIS._topBar.Xf      andAlso _
         GmY>=THIS._topBar.Yi      andAlso _
         GmY<=THIS._topBar.Yf   + 14   then
         '
         return TRUE
      else
         '
         return FALSE
      end if
   end function
   sub MAPPS.TestMouse()
      dim as integer gmX, gmY, gmWheel, gmBtn
      getMouse       gmX, gmY, gmWheel, gmBtn
      '
      'draw string (THIS.BckgndImgXi, THIS.BckgndImgYi), str(gmX) &".."& str(gmY)
      'draw string (THIS.BckgndImgXi, THIS.BckgndImgYi + 12), str(THIS._hasTopBarDragStarted)
      if gmX>=THIS.BckgndImgXi   andAlso _
         gmX<=THIS.BckgndImgXf   andAlso _
         gmY>=THIS.BckgndImgYi   andAlso _
         gmY<=THIS.BckgndImgYf   then
         if not THIS._hasMouseOver then THIS._hasMouseOver = TRUE
         if gmBtn>0 then
            if not THIS._hasMouseClick then THIS._hasMouseClick = TRUE
         else
            if THIS._hasMouseClick then THIS._hasMouseClick = FALSE
         end if
      else
         if THIS._hasMouseOver then THIS._hasMouseOver = FALSE
         if THIS._hasMouseClick then THIS._hasMouseClick = FALSE
      end if
      '
      select case THIS._hasTopBarMouseClick
         case TRUE
            THIS._hasTopBarDragStarted = TRUE
            '
            if not _
               ( gmX=(THIS._topBar.Xi + THIS._mouseXatDragStart)   andAlso _
                 gmY=(THIS._topBar.Yi + THIS._mouseYatDragStart) ) then
                'moving -> update position
                     if not THIS._hasMovedAtLeastOnce then THIS._hasMovedAtLeastOnce = TRUE
               ''note: uncomment below to allow drag
               'THIS._topBar.Xi   = gmX - THIS._mouseXatDragStart
               'THIS._topBar.Yi   = gmY - THIS._mouseYatDragStart
               'THIS.BckgndImgXi  = THIS._topBar.Xi
               'THIS.BckgndImgYi  = THIS._topBar.Yi
            else
               'not moving
               if not THIS._hasMovedAtLeastOnce then
                  THIS._hasTopBarDragStarted = FALSE
               else
                  THIS._hasTopBarDragStarted = TRUE
               end if
               '
               if THIS.MouseIsInTopBar(gmX, gmY) then
                  if not gmBtn>0 then
                     'reset mouseClick state
                     if THIS._hasMovedAtLeastOnce then THIS._hasMovedAtLeastOnce = FALSE
                     THIS._hasTopBarMouseClick = FALSE
                     THIS._mouseXatDragStart   = -1
                     THIS._mouseYatDragStart   = -1
                  end if
               else
                  'reset mouseOver state
                  THIS._hasTopBarMouseOver = FALSE
               end if
            end if
         case else
            'THIS._hasMouseClick==FALSE
            if THIS._hasTopBarDragStarted then
               THIS._hasTopBarDragStarted = FALSE
            end if
            '
            if THIS.MouseIsInTopBar(gmX, gmY) then
               if not THIS._hasTopBarMouseOver then THIS._hasTopBarMouseOver = TRUE
               '
               if gmBtn>0 then
                  THIS._hasTopBarMouseClick = TRUE
                  THIS._mouseXatDragStart   = gmX - THIS._topBar.Xi
                  THIS._mouseYatDragStart   = gmY - THIS._topBar.Yi
               end if
            else
               if THIS._hasTopBarMouseOver then THIS._hasTopBarMouseOver = FALSE
            end if
      end select
   end sub
   sub MAPPS.DrawHelper()
      THIS.TestMouse()
      '
      if THIS._hasMouseClick then
         THIS._borderColor => rgb(0,90,105)
      elseIf THIS._hasMouseOver then
         THIS._borderColor => rgb(0,90,105)
      else
         THIS._borderColor => rgb(0,90,105)
      end if
      '
      line  (THIS.BckgndImgXi, THIS.BckgndImgYi)- _
            (THIS.BckgndImgXf, THIS.BckgndImgYf), _
            THIS._borderColor, _
            b
      THIS._mainBackgroundImageBuffer.CheckerCreate THIS._borderColor, 20
   end sub
   sub MAPPS.DrawTopBar()
      THIS.TestMouse()
      '
      if not THIS._hasTopBarDragStarted then
         line  (THIS._topBar.Xi, THIS._topBar.Yf + 12)- _
               (THIS._topBar.Xf, THIS._topBar.Yf + 14), _
               rgb(0,190,200), _
               bf
      else
         line  (THIS._topBar.Xi, THIS._topBar.Yf + 12)- _
               (THIS._topBar.Xf, THIS._topBar.Yf + 14), _
               rgb(200,200,200), _
               b, _ 
               &b000101
      end if
   end sub
   sub MAPPS.RenderAppScreenFramework()
      if THIS._hasTopBarDragStarted then
         line  (0,0)- _
               (THIS._screenBuffer._w,  + THIS._screenBuffer._h), _
               rgb(255,0,255), _
               bf
         THIS.BuildArrayOfBorderLinePoint()
         THIS.BuildArrayOfTopBarFilledBoxPoint()
         THIS.BuildFullArrayOfPoint()
         'exit sub
      end if
      '
      dim as boolean   isInitiallyScreenLocked => djRedundo.IsScreenLocked
      if not isInitiallyScreenLocked then
         screenLock()
      end if
      put   (THIS.BckgndImgXi, THIS.BckgndImgYi), _
            THIS._mainBackgroundImageBuffer._img, _
            PSET
      '
      dim as AADOT  dot
      dot.RenderTarget (@THIS._screenBuffer)
      dot._alpha  => .65
      dim as single zPointScale   => 0.012
      '
      '/'
      THIS.QsortZ(   THIS._arrayOfPoint(), _
                     lBound(THIS._arrayOfPoint), _
                     uBound(THIS._arrayOfPoint) )
      '/
      '
      for pt as P3D ptr = @THIS._arrayOfPoint(lBound(THIS._arrayOfPoint)) to _
                                       @THIS._arrayOfPoint(uBound(THIS._arrayOfPoint))
         dim as single rz1 => (  THIS._mainAxis._z + _
                                 THIS._mainAxis._axisZ._z*pt->_z + _
                                 0*THIS._mainAxis._axisX._z*pt->_x + _
                                 0*THIS._mainAxis._axisY._z*pt->_y   )
         if rz1>0.1 then
            dim as single rz2 => THIS._zoomFactor/rz1
            dim as single y   => THIS._screenBuffer._midy - _
                                    rz2*( THIS._mainAxis._y + _
                                          THIS._mainAxis._axisY._y*pt->_y + _
                                          THIS._mainAxis._axisZ._y*pt->_z + _
                                          THIS._mainAxis._AxisX._y*pt->_x  )
            dim as single x   => THIS._screenBuffer._midx + _
                                    rz2*( THIS._mainAxis._x + _
                                          THIS._mainAxis._axisX._x*pt->_x + _
                                          THIS._mainAxis._axisY._x*pt->_y + _
                                          THIS._mainAxis._axisZ._x*pt->_z  )
            '
            dot._radius = zPointScale*rz2*pt->_radius
            dot.DrawAadot(x, y, pt->_color)
         end if
      next pt
      if not isInitiallyScreenLocked then
         screenUnlock()
      end if
       '
   end sub
   sub MAPPS.RenderAppScreenContent()
      '
   end sub
end namespace

nameSpace ttredundo
   #macro _ExitforOnEscapeKeyPressed
      if inkey=chr(27) then exit for
   #endMacro

   type SCREENTEST extends OBJECT
      'global variable container
      'storing screen parameter
      declare static sub TestScreen()
      static as integer  scrW
      static as integer  scrH
   end type 'SCREENTEST <- OBJECT
   dim as integer SCREENTEST.scrW       => -1
   dim as integer SCREENTEST.scrH       => -1
   sub SCREENTEST.TestScreen()
      screenInfo (SCREENTEST.scrW, SCREENTEST.scrH)
   end sub 'SCREENTEST.TestScreen()

   type INTERACTIONTEST extends SCREENTEST
      'global variable container
      'storing mouse/keyboard interaction
      declare static function TestMouse() as long
      declare static function TestKeyboard() as long
      static as long  gmX
      static as long  gmY
      static as long  gmBtn
      static as long  scanCode
   end type 'INTERACTIONTEST <- SCREENTEST <- OBJECT
   dim as long INTERACTIONTEST.gmX        => -1
   dim as long INTERACTIONTEST.gmY        => -1
   dim as long INTERACTIONTEST.gmBtn      => -1
   dim as long INTERACTIONTEST.scanCode   => -1
   function INTERACTIONTEST.TestMouse() as long
      return getMouse ( INTERACTIONTEST.gmX, _
                        INTERACTIONTEST.gmY, _
                        , _
                        INTERACTIONTEST.gmBtn   )
   end function 'LNG:=INTERACTIONTEST.TestMouse()
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
   end function 'LNG:=INTERACTIONTEST.TestMouse()
   
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
               if ._mouseClick=FALSE then ._mouseClick = TRUE
               if (TIMER - ._lastClickedTime)>._minClickTimeInterval then
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

'fb_stuff
nameSpace fbStuff
   enum _FBKEYWORDTYPE
      _singleAction
      _singleWaiter
      _singleStarter
      _singleEnder
      _multipleStarter
      _multipleEnder
   end enum

   type FBKEYWORD
         as _FBKEYWORDTYPE _keyWordType
         as string         _keyWord
   end type

   enum _COMBOTYPE
      _shortAct
      _shortWait
      _longAct
      _longWait
   end enum

   type FBKEYWORDCOMBO
      declare constructor()
      declare property IsEmptyCombo() as boolean
      declare function AddToCombo() as boolean
      declare sub DestroyCombo()
         as _COMBOTYPE  _comboType
         as FBKEYWORD   _combo(any)
   end type
end nameSpace

'fb_staff
nameSpace fbStaff
   type EQUIPMENT
      declare constructor()
      declare property  IsEmptyEquipment() as boolean
      declare property  IsCombo() as boolean
      declare sub AddEquipment(byref as fbStuff.FBKEYWORD, byref as fbStuff.FBKEYWORD, byref as fbStuff.FBKEYWORD)
      declare function MakeCombo() as boolean
         as boolean                 _isCombo
         as fbStuff.FBKEYWORDCOMBO  _equipmentCombo
         as fbStuff.FBKEYWORD       _equipmentKwArray(any)
   end type


   type FBCREWMEMBER
         as EQUIPMENT      _equipment
         as string         _crewMemberName
   end type
end nameSpace

'lab
nameSpace genLab
   type FBKEYWORDSLABBASKET
      declare constructor()
      declare sub AddKeywordToLabBasket(byref as fbStuff.FBKEYWORD)
         as fbStuff.FBKEYWORD    _arrayOfFbKeywords(any)
   end type
   constructor FBKEYWORDSLABBASKET()
      '
   end constructor
   sub FBKEYWORDSLABBASKET.AddKeywordToLabBasket(byref FbKw as fbStuff.FBKEYWORD)
      redim THIS._arrayOfFbKeywords(ubound(THIS._arrayOfFbKeywords) + 1)
      THIS._arrayOfFbKeywords(ubound(_arrayOfFbKeywords)) = FbKw
   end sub

   type FBKEYWORDSVESSELBASKET
      declare constructor()
      declare sub AddKeywordToVesselBasket(byref as fbStuff.FBKEYWORD)
         as fbStuff.FBKEYWORD    _arrayOfFbKeywords(any)
   end type
   constructor FBKEYWORDSVESSELBASKET()
      '
   end constructor
   sub FBKEYWORDSVESSELBASKET.AddKeywordToVesselBasket(byref FbKw as fbStuff.FBKEYWORD)
      redim THIS._arrayOfFbKeywords(ubound(THIS._arrayOfFbKeywords) + 1)
      THIS._arrayOfFbKeywords(ubound(_arrayOfFbKeywords)) = FbKw
   end sub
end nameSpace

'vessel
nameSpace vessel
   

end nameSpace

'codon
nameSpace codon
   

end nameSpace

'ground
nameSpace ground
   

end nameSpace

'-------------------------------------------------MAC.--------------------------
#macro _GAMESTARTPAGE()
   screenSet 0, 1
      scope
         dim as integer dskW, dskH
         screenControl fb.GET_DESKTOP_SIZE, dskW, dskH
         '
         dim as integer imgW, imgH
         imageInfo titlepic, imgW, imgH
         '
         put ((dskW - imgW)\2, (dskH - imgH)\2), titlepic, TRANS
         '
         line ((dskW - imgW)\2 + 40, (dskH + imgH)\2 - 16)-step(16*25, 10), rgba(200,100,0, 140), bf
         screenCopy 0, 1
         '
         dim as integer i
         for i = 1 to 17
            line ((dskW - imgW)\2 + 40, (dskH + imgH)\2 - 16 + 4*rnd() - 2)-step(i*25, 10), rgba(200,200,0, 80), bf
            line ((dskW - imgW)\2 + 40, (dskH + imgH)\2 - 16 + 4*rnd() - 2)-step(i*25, 10), rgba(0,200,100, 100), bf
            line ((dskW - imgW)\2 + 40, (dskH + imgH)\2 - 16)-step(i*25 + 1, 10 + 1), rgba(100,100,100, 200), b
            sleep 100
            screenCopy 0, 1
         next i
         i -= 1
         line ((dskW - imgW)\2 + 40, (dskH + imgH)\2 - 16)-step(i*25, 10), rgba(0,200,100, 100), bf
         line ((dskW - imgW)\2 + 40, (dskH + imgH)\2 - 16)-step(i*25, 10), rgba(100,200,100, 200), b
         line ((dskW - imgW)\2 + 40, (dskH + imgH)\2 - 16)-step(i*25, 10), rgba(0,200,100, 100), bf
         line ((dskW - imgW)\2 + 40, (dskH + imgH)\2 - 16)-step(i*25, 10), rgba(100,200,100, 200), b
         line ((dskW - imgW)\2 + 40, (dskH + imgH)\2 - 16)-step(i*25, 10), rgba(0,140,200, 100), bf
         line ((dskW - imgW)\2 + 40, (dskH + imgH)\2 - 16)-step(i*25, 10), rgba(100,200,100, 200), b
      end scope
   screenCopy 0, 1
   sleep 800
   '
   summaryStartTime  = TIMER
   SCENE.state = SCENE._STATE._summary
#endMacro

#macro _SUMMARYPAGE()
   screenSet 0, 1
      cls
      mono.RenderAppScreenFramework()
      mono.DrawHelper()
      mono.DrawTopBar()
      
      'summary menu interface
      summary_LabBtn.DrawButton()
      summary_VesselBtn.DrawButton()
      summary_QuitBtn.DrawButton()
      
      line ((dskW - 390)/2, 280)-step(400,400), rgba(50,250,255, 20), bf
      
      'test for mouseover button
      if summary_LabBtn._mouseOver then
         put ((dskW - 360)/2,300), biohazardpic, trans
         line (2,dskH - 75)-(dskW - 2, dskH - 2), rgba(20,20,20,220), bf
         draw string (10, dskH - 64), "GENETICS FACILITY", 0, starter2
      end if
      if summary_VesselBtn._mouseOver then
         put ((dskW - 360)/2,300), hangarPic, trans
         line (2,dskH - 75)-(dskW - 2, dskH - 2), rgba(20,20,20,220), bf
         draw string (10, dskH - 64), "CREW HANGARS", 0, starter2
      end if
      if summary_QuitBtn._mouseOver then
         '
      end if
      
      
      if (TIMER - summaryStartTime)<1 then
         dim as integer imgW, imgH
         imageInfo titlepic, imgW, imgH
         put ((dskW - imgW)\2, (dskH - imgH)\2), titlepic, TRANS
         line ((dskW - imgW)\2 + 40, (dskH + imgH)\2 - 16)-step(17*25, 10), rgba(0,200,100, 100), bf
         line ((dskW - imgW)\2 + 40, (dskH + imgH)\2 - 16)-step(17*25, 10), rgba(100,200,100, 200), b
         line ((dskW - imgW)\2 + 40, (dskH + imgH)\2 - 16)-step(17*25, 10), rgba(0,200,100, 100), bf
         line ((dskW - imgW)\2 + 40, (dskH + imgH)\2 - 16)-step(17*25, 10), rgba(100,200,100, 200), b
         line ((dskW - imgW)\2 + 40, (dskH + imgH)\2 - 16)-step(17*25, 10), rgba(0,140,200, 100), bf
         line ((dskW - imgW)\2 + 40, (dskH + imgH)\2 - 16)-step(17*25, 10), rgba(100,200,100, 200), b
      end if
   screenCopy 0, 1
#endMacro

'-------------------------------------------------MAIN--------------------------
'-------------------------------------------------INIT--------------------------
#define _PLAY  _void
SCENE.state => SCENE._STATE._gameStart   ''change this to start on another scene
'' dev.note:   build fbkeywordlist          at play init
''             build fbkeywordstafflist     at play init

randomize TIMER
'(*)graphics and fonts
'...graphics initialisation
   width 140, 64
   dim as integer dskW, dskH
   screenControl fb.GET_DESKTOP_SIZE, dskW, dskH
   'dskW = 800
   'dskH = 600
   width 14, 12
   dim as dafhiRedundo.MONOAPPSCREEN   mono => dafhiRedundo.MONOAPPSCREEN(dskW, dskH)
   mono._mainAxis._z = .1
   mono._zoomFactor  = .97

'...fonts initialisation
   dodifont._Createfont    starter, 1.5, RGB(100,210,180), 20
   dodifont._Createfont    starter2, 4, RGB(230,180,110), 10
   dodifont._Createfont    starter3, 1.9, RGB(190,100,180), 2
   dodifont._Createfont    starter4, 5, RGB(100,220,080), 14

'...images loading
titlepic       => CreditFBDoc.BMPlOAD(curDir() & "\pics\title.bmp")
biohazardpic   => CreditFBDoc.BMPlOAD(curDir() & "\pics\biohazard.bmp")
hangarPic      => CreditFBDoc.BMPlOAD(curDir() & "\pics\hangar.bmp")
quitPic        => CreditFBDoc.BMPlOAD(curDir() & "\pics\quitStrange.bmp")

'...some gui control and settings
dim as integer       summaryStartTime
dim as ttredundo.DBUTTON   summary_LabBtn => ttredundo.DBUTTON((dskW - 14*18)/2, _
                                                               100, _
                                                               14*18, _
                                                               32, _
                                                               "GO TO GENETICS LAB", _
                                                               ttredundo.DBUTTON._BTNBEHAVIOUR._useDelay, _
                                                               .5)
with summary_LabBtn
   ._btnColor              => rgba(100,200,000, 200)
   ._btnMouseOverColor     => rgba(000,100,100, 200)
   ._btnMouseClickColor    => rgba(000,100,000, 100)
end with
dim as ttredundo.DBUTTON   summary_VesselBtn => ttredundo.DBUTTON((dskW - 14*18)/2, _
                                                               144, _
                                                               14*18, _
                                                               32, _
                                                               "GET TO CREW HANGAR", _
                                                               ttredundo.DBUTTON._BTNBEHAVIOUR._useDelay, _
                                                               .5)
with summary_VesselBtn
   ._btnColor              => rgba(100,200,000, 200)
   ._btnMouseOverColor     => rgba(000,100,100, 200)
   ._btnMouseClickColor    => rgba(000,100,000, 100)
end with
dim as ttredundo.DBUTTON   summary_QuitBtn => ttredundo.DBUTTON((dskW - 14*18)/2, _
                                                               188, _
                                                               14*18, _
                                                               32, _
                                                               "   QUIT THE GAME", _
                                                               ttredundo.DBUTTON._BTNBEHAVIOUR._useDelay, _
                                                               .5)
with summary_QuitBtn
   ._btnColor              => rgba(200,100,000, 200)
   '._btnMouseOverColor     => rgba(000,100,100, 200)
   '._btnMouseClickColor    => rgba(000,100,000, 100)
end with


'(**)


'-------------------------------------------------LOOP--------------------------
do
   ttredundo.SCREENTEST.TestScreen()
   ttredundo.INTERACTIONTEST.TestMouse()
   ttredundo.INTERACTIONTEST.TestKeyboard()
   
   select case SCENE.state
      case SCENE._STATE._gameStart
         _PLAY _GAMESTARTPAGE()
      case SCENE._STATE._summary
         _PLAY _SUMMARYPAGE()
      case SCENE._STATE._laboratory
         
      case SCENE._STATE._travelRoom
         
      case SCENE._STATE._groundScenery
      
      case SCENE._STATE._endOfGame
         
      case SCENE._STATE._gameExit
         
   end select
   '
   'exit button
   screenSet 0, 1
      if (ttredundo.INTERACTIONTEST.gmBtn=1) andAlso _ 
         sqr((ttredundo.INTERACTIONTEST.gmx - (mono.BckgndImgXf - 32))^2 + (ttredundo.INTERACTIONTEST.gmy - (mono.BckgndImgYi + 20))^2)<10 then
         line (2,dskH - 75)-(dskW - 2, dskH - 2), rgba(20,20,20,220), bf
         draw string (10, dskH - 64), "GAME ENDED", 0, starter2
         screenCopy 0, 1
         exit do
      end if
      circle (mono.BckgndImgXf - 32, mono.BckgndImgYi + 18), 10, rgba(200,0,0, 120), , , , f
   screenCopy 0, 1
   '
   sleep 15
   _SLEEP1MS()
loop 'until inkey()=chr(27)

'-------------------------------------------------CLEA--------------------------


'-------------------------------------------------END.--------------------------
getKey()

'(eof)