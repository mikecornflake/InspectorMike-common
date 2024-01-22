unit WGS84;

{$MODE Delphi}

{
2022-10-10 Added "IgnoreGeodeticOrientation" to prevent swapping E/N and
           making N negative for southern hemisphere

2013-08-21 Rättat initiering av medelMeridian till '2.5 gon V' om den är angiven
           som '' eller ' '
2013-07-30 Lagt till initiering av medelMeridian till '2.5 gon V' om den är angiven
           som '' eller ' '
2011-10-28 Lagt till initiering av medelMeridian till '2.5 gon V' om den inte är angiven
}

interface

uses
  {Windows,} Messages, SysUtils, Classes, Graphics, Controls, Math;

const
  XTranslToRR92 : extended = -419.375;
  YTranslToRR92 : extended = -99.352;
  ZTranslToRR92 : extended = -591.349;
  skalfToRR92   : extended = 0.00000099496;
  XwToRR92      : extended = 0.000004123137;
  YwToRR92      : extended = 0.000008810252;
  ZwToRR92      : extended = -0.000038117239;

  XTranslToSWEREF93 : extended = 419.3836;
  YTranslToSWEREF93 : extended = 99.3335;
  ZTranslToSWEREF93 : extended = 591.3451;
  skalfToSWEREF93   : extended = -0.00000099496;
  XwToSWEREF93      : extended = -0.000004122802;
  YwToSWEREF93      : extended = -0.000008810408;
  ZwToSWEREF93      : extended = 0.000038117205;

type
  TrecLatLon =
  record
    Lat,Lon : double;
    OK      : boolean;
  end;
  TrecUTM    =
  record
    X,Y       : double;
    fuseau    : integer;
    southhemi : boolean;
    CharLat   : char;
    OK        : boolean;
  end;
  meridian2rad = record
    system : string;
    grad, minut, sekund : string;
    radianer : extended;
  end;

  { TWGS84 }

  TWGS84 = class(TObject)
  private
    FIgnoreGeodeticOrientation: Boolean;
    { Private declarations }
    procedure InitieraGPSSWEREF99RT90Variabler;

    procedure SWEREF93ToRR92xyz(xSWEREF93,ySWEREF93,zSWEREF93 : extended;
      var xRR92,yRR92,zRR92 : extended);
    procedure RR92ToSWEREF93xyz(xRR92,yRR92,zRR92 : extended;
      var xSWEREF93,ySWEREF93,zSWEREF93 : extended);

    procedure FiLambdaH2XYZ(Fi, Lambda, H : extended;
      var x, y, z : extended; system : string);
    procedure XYZ2FiLambdaH(x, y, z : extended;
      var Fi, Lambda, H : extended; system : string);

    procedure PlanaXY2FiLambdaH(x, y : extended;
      var Fi, Lambda, H : extended; system, medelMeridian : string);
    procedure FiLambdaH2PlanaXY(Fi, Lambda, H : extended;
      var x, y : extended; system, medelMeridian : string);

    function fakultet(i : integer) : integer;

    function GradMinutSekund2Radianer(Grad, Minut, Sekund : string) : extended;
    function BytTillAktuellDecimalSeparator(strangTal : string) : string;

    procedure FiLambdaH2PlanaXYSWEREF99(Fi, Lambda, H: extended; var x,
      y: extended; system: string);
    procedure PlanaXY2FiLambdaHSWEREF99(x, y: extended; var Fi, Lambda,
      H: extended; system: string);
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;

    procedure WGS84ToSWEREF99(Fi, Lambda, H : extended;
      var x, y : extended);
    procedure WGS84AsDegreeAndDecimalMinuteToSWEREF99(Fi, Lambda, H : extended;
      var x, y : extended);
    procedure SWEREF99ToWGS84(x, y : extended;
      var Fi, Lambda, H : extended);
    procedure SWEREF99ToWGS84AsDegreeAndDecimalMinute(x, y: extended;
      var Fi, Lambda, H: extended);
    procedure RT90ToSWEREF99(xRT90, yRT90 : extended;
      var xSWEREF99, ySWEREF99 : extended);
    procedure SWEREF99ToRT90(xSWEREF99, ySWEREF99 : extended;
      var xRT90, yRT90 : extended);
    procedure GPSToRT90(Fi, Lambda, H : extended; medelMeridian : string;
      var x, y : extended);
    procedure GPSAsDegreeAndDecimalMinuteToRT90(Fi, Lambda, H : extended; medelMeridian : string;
      var x, y : extended);
    procedure RT90ToGPS(x, y : extended; medelMeridian : string;
      var Fi, Lambda, H : extended);
    procedure RT90ToGPSAsDegreeAndDecimalMinute(x, y: extended;
      medelMeridian: string; var Fi, Lambda, H: extended);
    procedure WGS84ToUTM(LatLon:TrecLatLon; var UTM:TrecUTM);
    procedure UTMToWGS84(UTM:TrecUTM; var latlon:TrecLatLon);

    property IgnoreGeodeticOrientation: Boolean read FIgnoreGeodeticOrientation write FIgnoreGeodeticOrientation;
  end;

var
  cosXToRR92, sinXToRR92, cosYToRR92, sinYToRR92, cosZToRR92, sinZToRR92 : extended;
  cosXToSWEREF93, sinXToSWEREF93, cosYToSWEREF93, sinYToSWEREF93, cosZToSWEREF93, sinZToSWEREF93 : extended;
  a1RR92, a2RR92, a3RR92, b1RR92, b2RR92, b3RR92, c1RR92, c2RR92, c3RR92 : extended;
  a1SWEREF93, a2SWEREF93, a3SWEREF93, b1SWEREF93, b2SWEREF93, b3SWEREF93, c1SWEREF93, c2SWEREF93, c3SWEREF93 : extended;
  xSWEREF93,ySWEREF93,zSWEREF93, xRR92,yRR92,zRR92, xRT90, yRT90 : extended;

  icode : integer;
  fiSWEREF93, lambdaSWEREF93, hSWEREF93 : extended;
  fiRR92, lambdaRR92, hRR92 : extended;

  falseEastingSWEREF93 : extended;
  falseEastingSWEREF99 : extended;
  falseNorthingSWEREF99 : extended;

  lillaHBessel, TvarKrokRadieBessel, excentricitetKvadratenBessel : extended;
  avplattningBessel, halvaStoraxelnBessel : extended;

  lillaHSWEREF93, TvarKrokRadieSWEREF93, excentricitetKvadratenSWEREF93 : extended;
  avplattningSWEREF93, halvaStoraxelnSWEREF93 : extended;

  lillaHSWEREF99, TvarKrokRadieSWEREF99, excentricitetKvadratenSWEREF99 : extended;
  avplattningSWEREF99, halvaStoraxelnSWEREF99 : extended;
  medelMeridianFaktorSWEREF93, medelMeridianFaktorSWEREF99 : extended;
  medelMeridian : string;

  medelMeridianFaktorRT90WGS84 : extended; // RT90 <--> WGS84-ellipsoiden
  falseNorthingRT90WGS84 : extended;          // RT90 <--> WGS84-ellipsoiden
  falseEastingRT90WGS84 : extended;        // RT90 <--> WGS84-ellipsoiden

  Medelmeridianer : array[1..6] of meridian2rad;
  MedelmeridianRT90WGS84 : extended;
  MedelmeridianSWEREF99 : extended;

implementation

{ TWGS84 }

constructor TWGS84.Create;
begin
  inherited;

  InitieraGPSSWEREF99RT90Variabler;
end;

destructor TWGS84.Destroy;
begin
  inherited;
end;

procedure TWGS84.InitieraGPSSWEREF99RT90Variabler;
begin
// Beräkna cos- och sin-värden för vinklar TILL RR92
  cosXToRR92:=cos(XwToRR92); sinXToRR92:=sin(XwToRR92);
  cosYToRR92:=cos(YwToRR92); sinYToRR92:=sin(YwToRR92);
  cosZToRR92:=cos(ZwToRR92); sinZToRR92:=sin(ZwToRR92);

// Beräkna matriselement för transformering TILL RR92 a,b,c - rad1,2,3
  a1RR92:=cosYToRR92*cosZToRR92;
  a2RR92:=cosXToRR92*sinZToRR92+sinXToRR92*sinYToRR92*cosZToRR92;
  a3RR92:=-cosXToRR92*sinYToRR92*cosZToRR92+sinXToRR92*sinZToRR92;
  b1RR92:=-cosYToRR92*sinZToRR92;
  b2RR92:=cosXToRR92*cosZToRR92-sinXToRR92*sinYToRR92*sinZToRR92;
  b3RR92:=sinXToRR92*cosZToRR92+cosXToRR92*sinYToRR92*sinZToRR92;
  c1RR92:=sinYToRR92;
  c2RR92:=-sinXToRR92*cosYToRR92;
  c3RR92:=cosXToRR92*cosYToRR92;

// Beräkna cos- och sin-värden för vinklar TILL SWEREF93
  cosXToSWEREF93:=cos(XwToSWEREF93); sinXToSWEREF93:=sin(XwToSWEREF93);
  cosYToSWEREF93:=cos(YwToSWEREF93); sinYToSWEREF93:=sin(YwToSWEREF93);
  cosZToSWEREF93:=cos(ZwToSWEREF93); sinZToSWEREF93:=sin(ZwToSWEREF93);

// Beräkna matriselement för transformering TILL SWEREF93 a,b,c - rad1,2,3
  a1SWEREF93:=cosYToSWEREF93*cosZToSWEREF93;
  a2SWEREF93:=cosXToSWEREF93*sinZToSWEREF93+sinXToSWEREF93*sinYToSWEREF93*cosZToSWEREF93;
  a3SWEREF93:=-cosXToSWEREF93*sinYToSWEREF93*cosZToSWEREF93+sinXToSWEREF93*sinZToSWEREF93;
  b1SWEREF93:=-cosYToSWEREF93*sinZToSWEREF93;
  b2SWEREF93:=cosXToSWEREF93*cosZToSWEREF93-sinXToSWEREF93*sinYToSWEREF93*sinZToSWEREF93;
  b3SWEREF93:=sinXToSWEREF93*cosZToSWEREF93+cosXToSWEREF93*sinYToSWEREF93*sinZToSWEREF93;
  c1SWEREF93:=sinYToSWEREF93;
  c2SWEREF93:=-sinXToSWEREF93*cosYToSWEREF93;
  c3SWEREF93:=cosXToSWEREF93*cosYToSWEREF93;

// Beräkna variabler för transformation mellan geocentriska kartesiska
// koordinater och geodetiska koordinater
  halvaStoraxelnBessel:=6377397.155;
  avplattningBessel:=1/299.1528128;
  excentricitetKvadratenBessel:=avplattningBessel*(2-avplattningBessel);
//  excentricitetKvadratenBessel:=0.0066743722318;

// Beräkna variabler för transformation mellan geodetiska koordinater och
// geocentriska kartesiska koordinater
  halvaStoraxelnSWEREF93:=6378137;
  avplattningSWEREF93:=1/298.257222101;
  excentricitetKvadratenSWEREF93:=avplattningSWEREF93*(2-avplattningSWEREF93);

  medelMeridianFaktorSWEREF93:=1.0000;
  falseEastingSWEREF93:=1500000.0;

  MedelmeridianSWEREF99 := GradMinutSekund2Radianer('15', '0', '0');
  halvaStoraxelnSWEREF99:=6378137;
  avplattningSWEREF99:=1/298.257222101;
  excentricitetKvadratenSWEREF99:=avplattningSWEREF99*(2-avplattningSWEREF99);

  MedelmeridianRT90WGS84 := GradMinutSekund2Radianer('15', '48', '22.624306');
  medelMeridianFaktorRT90WGS84:=1.00000561024; // RT90 <--> WGS84-ellipsoiden
  falseNorthingRT90WGS84 := -667.711;          // RT90 <--> WGS84-ellipsoiden
  falseEastingRT90WGS84 := 1500064.274;        // RT90 <--> WGS84-ellipsoiden

  medelMeridianFaktorSWEREF99:=0.9996;         // SWEREF99TM <--> WGS84-ellipsoiden
  falseNorthingSWEREF99 := 0;                  // SWEREF99TM <--> WGS84-ellipsoiden
  falseEastingSWEREF99 := 500000;              // SWEREF99TM <--> WGS84-ellipsoiden

// Initiera tabell för översättning av medelmeridianbeteckning till radianer
  Medelmeridianer[1].system:='7.5 gon V';
  Medelmeridianer[1].grad:='11';
  Medelmeridianer[1].minut:='18';
  Medelmeridianer[1].sekund:='29.8';
  Medelmeridianer[1].radianer:=
    GradMinutSekund2Radianer(Medelmeridianer[1].grad,
                             Medelmeridianer[1].minut,
                             Medelmeridianer[1].sekund);

  Medelmeridianer[2].system:='5.0 gon V';
  Medelmeridianer[2].grad:='13';
  Medelmeridianer[2].minut:='33';
  Medelmeridianer[2].sekund:='29.8';
  Medelmeridianer[2].radianer:=
    GradMinutSekund2Radianer(Medelmeridianer[2].grad,
                             Medelmeridianer[2].minut,
                             Medelmeridianer[2].sekund);

  Medelmeridianer[3].system:='2.5 gon V';
  Medelmeridianer[3].grad:='15';
  Medelmeridianer[3].minut:='48';
  Medelmeridianer[3].sekund:='29.8';
  Medelmeridianer[3].radianer:=
    GradMinutSekund2Radianer(Medelmeridianer[3].grad,
                             Medelmeridianer[3].minut,
                             Medelmeridianer[3].sekund);

  Medelmeridianer[4].system:='0 gon';
  Medelmeridianer[4].grad:='18';
  Medelmeridianer[4].minut:='3';
  Medelmeridianer[4].sekund:='29.8';
  Medelmeridianer[4].radianer:=
    GradMinutSekund2Radianer(Medelmeridianer[4].grad,
                             Medelmeridianer[4].minut,
                             Medelmeridianer[4].sekund);

  Medelmeridianer[5].system:='2.5 gon O';
  Medelmeridianer[5].grad:='20';
  Medelmeridianer[5].minut:='18';
  Medelmeridianer[5].sekund:='29.8';
  Medelmeridianer[5].radianer:=
    GradMinutSekund2Radianer(Medelmeridianer[5].grad,
                             Medelmeridianer[5].minut,
                             Medelmeridianer[5].sekund);

  Medelmeridianer[6].system:='5.0 gon O';
  Medelmeridianer[6].grad:='22';
  Medelmeridianer[6].minut:='33';
  Medelmeridianer[6].sekund:='29.8';
  Medelmeridianer[6].radianer:=
    GradMinutSekund2Radianer(Medelmeridianer[6].grad,
                             Medelmeridianer[6].minut,
                             Medelmeridianer[6].sekund);

  FIgnoreGeodeticOrientation := False;
end;

procedure TWGS84.WGS84ToSWEREF99(Fi, Lambda, H : extended;
  var x, y : extended);
begin
  FiLambdaH2PlanaXYSWEREF99(Fi, Lambda, H, x, y, 'SWEREF99');
end;

procedure TWGS84.SWEREF99ToWGS84(x, y : extended;
  var Fi, Lambda, H : extended);
begin
  PlanaXY2FiLambdaHSWEREF99(x, y, Fi, Lambda, H, 'SWEREF99');
end;

procedure TWGS84.RT90ToSWEREF99(xRT90, yRT90 : extended;
  var xSWEREF99, ySWEREF99 : extended);
var
  Fi, Lambda, H : extended;
begin
  PlanaXY2FiLambdaHSWEREF99(xRT90, yRT90, Fi, Lambda, H,
    'RT90');
  FiLambdaH2PlanaXYSWEREF99(Fi, Lambda, H, xSWEREF99, ySWEREF99,
    'SWEREF99');
end;

procedure TWGS84.SWEREF99ToRT90(xSWEREF99, ySWEREF99 : extended;
  var xRT90, yRT90 : extended);
var
  Fi, Lambda, H : extended;
begin
  PlanaXY2FiLambdaHSWEREF99(xSWEREF99, ySWEREF99, Fi, Lambda, H, 'SWEREF99');
  FiLambdaH2PlanaXYSWEREF99(Fi, Lambda, H, xRT90, yRT90, 'RT90');
end;

procedure TWGS84.GPSToRT90(Fi, Lambda, H : extended; medelMeridian : string;
  var x, y : extended);
begin
  if (medelMeridian = '') or (medelMeridian = ' ') then
    medelMeridian := '2.5 gon V';

// Gå från SWEREF93 FiLambda --> SWEREF93 XYZ
  FiLambdaH2XYZ(Fi, Lambda, H,
    xSWEREF93,ySWEREF93,zSWEREF93, 'SWEREF93');

// Gå från SWEREF93 XYZ --> RR92 XYZ
  SWEREF93ToRR92xyz(xSWEREF93,ySWEREF93,zSWEREF93, xRR92,yRR92,zRR92);

// Gå från RR92 XYZ --> RR92 FiLambda
  XYZ2FiLambdaH(xRR92,yRR92,zRR92, fiRR92, lambdaRR92, hRR92, 'Bessel');

// Gå från RR92 FiLambda --> RT90 xy
  FiLambdaH2PlanaXY(fiRR92, lambdaRR92, hRR92, xRT90, yRT90,
    'Bessel', medelMeridian);

  x := xRT90; y := yRT90;
end;

procedure TWGS84.GPSAsDegreeAndDecimalMinuteToRT90(Fi, Lambda, H: extended;
  medelMeridian: string; var x, y: extended);
var
  minute : extended;
  strMinute : string;
begin
  if (medelMeridian = '') or (medelMeridian = ' ') then
    medelMeridian := '2.5 gon V';

  strMinute := FloatToStr(Fi);
  strMinute := Copy(strMinute, 3, Length(strMinute) - 2);
  minute := StrToFloat(strMinute);
  Fi := Trunc(Fi / 100);
  Fi := Fi + minute / 60;

  strMinute := FloatToStr(Lambda);
  strMinute := Copy(strMinute, 3, Length(strMinute) - 2);
  minute := StrToFloat(strMinute);
  Lambda := Trunc(Lambda / 100);
  Lambda := Lambda + minute / 60;

  GPSToRT90(Fi, Lambda, H, medelMeridian, x, y);
end;

procedure TWGS84.RT90ToGPS(x, y : extended; medelMeridian : string;
  var Fi, Lambda, H : extended);
begin
  if (medelMeridian = '') or (medelMeridian = ' ') then
    medelMeridian := '2.5 gon V';

// Gå från RT90 XYZ --> RR92 FiLambda
  xRT90 := x; yRT90 := y;
  PlanaXY2FiLambdaH(xRT90, yRT90, fiRR92, lambdaRR92, hRR92,
    'Bessel', medelMeridian);

// Gå från RR92 FiLambda --> RR92 XYZ
  FiLambdaH2XYZ(fiRR92, lambdaRR92, hRR92, xRR92,yRR92,zRR92,  'Bessel');

// Gå från RR92 XYZ --> SWEREF93 XYZ
  RR92ToSWEREF93xyz(xRR92,yRR92,zRR92, xSWEREF93,ySWEREF93,zSWEREF93);

// Gå från SWEREF93 XYZ --> SWEREF93 FiLambda
  XYZ2FiLambdaH(xSWEREF93,ySWEREF93,zSWEREF93,
    fiSWEREF93, lambdaSWEREF93, hSWEREF93, 'SWEREF93');
  Fi := fiSWEREF93;
  Lambda := lambdaSWEREF93;
  H := hSWEREF93;
end;

procedure TWGS84.RT90ToGPSAsDegreeAndDecimalMinute(x, y: extended;
  medelMeridian: string; var Fi, Lambda, H: extended);
var
  minute : extended;
begin
  if (medelMeridian = '') or (medelMeridian = ' ') then
    medelMeridian := '2.5 gon V';

  RT90ToGPS(x, y, medelMeridian, Fi, Lambda, H);

  minute := Frac(Fi) * 60;
  minute := Round(minute * 10000) / 10000;
  Fi := Trunc(Fi) * 100 + minute;

  minute := Frac(Lambda) * 60;
  minute := Round(minute * 10000) / 10000;
  Lambda := Trunc(Lambda) * 100 + minute;
end;

procedure TWGS84.SWEREF93ToRR92xyz(xSWEREF93,ySWEREF93,zSWEREF93 : extended;
  var xRR92,yRR92,zRR92 : extended);
begin
  xRR92:=(1+skalfToRR92)*(a1RR92*xSWEREF93+a2RR92*ySWEREF93+a3RR92*zSWEREF93)+XTranslToRR92;
  yRR92:=(1+skalfToRR92)*(b1RR92*xSWEREF93+b2RR92*ySWEREF93+b3RR92*zSWEREF93)+YTranslToRR92;
  zRR92:=(1+skalfToRR92)*(c1RR92*xSWEREF93+c2RR92*ySWEREF93+c3RR92*zSWEREF93)+ZTranslToRR92;
end;

procedure TWGS84.RR92ToSWEREF93xyz(xRR92,yRR92,zRR92 : extended;
  var xSWEREF93,ySWEREF93,zSWEREF93 : extended);
begin
  xSWEREF93:=(1+skalfToSWEREF93)*(a1SWEREF93*xRR92+a2SWEREF93*yRR92+a3SWEREF93*zRR92)+XTranslToSWEREF93;
  ySWEREF93:=(1+skalfToSWEREF93)*(b1SWEREF93*xRR92+b2SWEREF93*yRR92+b3SWEREF93*zRR92)+YTranslToSWEREF93;
  zSWEREF93:=(1+skalfToSWEREF93)*(c1SWEREF93*xRR92+c2SWEREF93*yRR92+c3SWEREF93*zRR92)+ZTranslToSWEREF93;
end;

procedure TWGS84.FiLambdaH2XYZ(Fi, Lambda, H : extended;
  var x, y, z : extended; system : string);
var
  sinFi : extended;
begin
  Fi:=DegToRad(Fi);
  Lambda:=DegToRad(Lambda);
  sinFi:=sin(Fi);

  if system = 'Bessel' then
  begin
    TvarKrokRadieBessel:=halvaStoraxelnBessel/
      sqrt(1-excentricitetKvadratenBessel*sinFi*sinFi);
    x:=(TvarKrokRadieBessel+H)*cos(Fi)*cos(Lambda);
    y:=(TvarKrokRadieBessel+H)*cos(Fi)*sin(Lambda);
    z:=(TvarKrokRadieBessel*(1-excentricitetKvadratenBessel)+H)*sin(Fi);
  end
  else
  begin
    TvarKrokRadieSWEREF93:=halvaStoraxelnSWEREF93/
      sqrt(1-excentricitetKvadratenSWEREF93*sinFi*sinFi);
    x:=(TvarKrokRadieSWEREF93+H)*cos(Fi)*cos(Lambda);
    y:=(TvarKrokRadieSWEREF93+H)*cos(Fi)*sin(Lambda);
    z:=(TvarKrokRadieSWEREF93*(1-excentricitetKvadratenSWEREF93)+H)*sin(Fi);
  end;
end;

procedure TWGS84.XYZ2FiLambdaH(x, y, z : extended;
  var Fi, Lambda, H : extended; system : string);
var
  Theta, sinTheta, cosTheta, p, tanFi, sinFi : extended;
begin
  if system = 'Bessel' then
  begin
    p:=sqrt(x*x+y*y);
    Theta:=ArcTan(z/(p*sqrt(1-excentricitetKvadratenBessel)));
    sinTheta:=sin(Theta); cosTheta:=cos(Theta);
    tanFi:=(z+((halvaStoraxelnBessel*excentricitetKvadratenBessel)/
              sqrt(1-excentricitetKvadratenBessel))*
              sinTheta*sinTheta*sinTheta)/
              (p-halvaStoraxelnBessel*excentricitetKvadratenBessel*
              cosTheta*cosTheta*cosTheta);
    Fi:=ArcTan(tanFi);
    sinFi:=sin(Fi);
    TvarKrokRadieBessel:=halvaStoraxelnBessel/
      sqrt(1-excentricitetKvadratenBessel*sinFi*sinFi);
    H:=p/cos(Fi) - TvarKrokRadieBessel;
    Lambda:=ArcTan(y/x);
  end
  else
  begin
    p:=sqrt(x*x+y*y);
    Theta:=ArcTan(z/(p*sqrt(1-excentricitetKvadratenSWEREF93)));
    sinTheta:=sin(Theta); cosTheta:=cos(Theta);
    tanFi:=(z+((halvaStoraxelnSWEREF93*excentricitetKvadratenSWEREF93)/
              sqrt(1-excentricitetKvadratenSWEREF93))*
              sinTheta*sinTheta*sinTheta)/
              (p-halvaStoraxelnSWEREF93*excentricitetKvadratenSWEREF93*
              cosTheta*cosTheta*cosTheta);
    Fi:=ArcTan(tanFi);
    sinFi:=sin(Fi);
    TvarKrokRadieSWEREF93:=halvaStoraxelnSWEREF93/
      sqrt(1-excentricitetKvadratenSWEREF93*sinFi*sinFi);
    H:=p/cos(Fi) - TvarKrokRadieSWEREF93;
    Lambda:=ArcTan(y/x);
  end;
  Fi:=RadToDeg(Fi);
  Lambda:=RadToDeg(Lambda);
end;

procedure TWGS84.PlanaXY2FiLambdaH(x, y : extended;
  var Fi, Lambda, H : extended; system, medelMeridian : string);
var
  n, nKvadrat, aTak, Meridian, deltaLambda : extended;
  Aeta, Ksi, AetaPrim, KsiPrim : extended;
  FiIso, deltaLambdaIso, sinFiIso : extended;
  d1, d2, d3, d4, A, B, C, D, e2, e4, e6, e8 : extended;
begin
  try
    if medelMeridian = '7.5 gon V' then
      Meridian:=Medelmeridianer[1].radianer
    else if medelMeridian = '5.0 gon V' then
      Meridian:=Medelmeridianer[2].radianer
    else if medelMeridian = '2.5 gon V' then
      Meridian:=Medelmeridianer[3].radianer
    else if medelMeridian = '0 gon' then
      Meridian:=Medelmeridianer[4].radianer
    else if medelMeridian = '2.5 gon O' then
      Meridian:=Medelmeridianer[5].radianer
    else if medelMeridian = '5.0 gon O' then
      Meridian:=Medelmeridianer[6].radianer;

    y:=y-falseEastingSWEREF93;

    if system = 'Bessel' then
    begin
      n:=avplattningBessel/(2-avplattningBessel);
      nKvadrat:=n*n;
      aTak:=(halvaStoraxelnBessel/(1+n))*
              (1+(1/4)*nKvadrat+(1/64)*nKvadrat*nKvadrat);

      Ksi:=x/(medelMeridianFaktorSWEREF93*aTak);
      Aeta:=y/(medelMeridianFaktorSWEREF93*aTak);

      d1:=(1/2)*n-(2/3)*nKvadrat+(37/96)*n*nKvadrat-(1/360)*nKvadrat*nKvadrat;
      d2:=(1/48)*nKvadrat+(1/15)*n*nKvadrat-(437/1440)*nKvadrat*nKvadrat;
      d3:=(17/480)*n*nKvadrat-(37/840)*nKvadrat*nKvadrat;
      d4:=(4397/161280)*nKvadrat*nKvadrat;

      e2:=excentricitetKvadratenBessel;
      e4:=e2*e2;
      e6:=e2*e4;
      e8:=e2*e6;
      A:=e2+e4+e6+e8;
      B:=-(1/6)*(7*e4+17*e6+30*e8);
      C:=(1/120)*(224*e6+889*e8);
      D:=-(1/1260)*(4279*e8);

      KsiPrim:=Ksi-
        d1*sin(2*Ksi)*cosh(2*Aeta)-
        d2*sin(4*Ksi)*cosh(4*Aeta)-
        d3*sin(6*Ksi)*cosh(6*Aeta)-
        d4*sin(8*Ksi)*cosh(8*Aeta);
      AetaPrim:=Aeta-
        d1*cos(2*Ksi)*sinh(2*Aeta)-
        d2*cos(4*Ksi)*sinh(4*Aeta)-
        d3*cos(6*Ksi)*sinh(6*Aeta)-
        d4*cos(8*Ksi)*sinh(8*Aeta);

      FiIso:=ArcSin(sin(KsiPrim)/cosh(AetaPrim));
      deltaLambdaIso:=ArcTan(sinh(AetaPrim)/cos(KsiPrim));
      deltaLambda:=deltaLambdaIso;

      sinFiIso:=sin(FiISo);
      Fi:=FiIso + sinFiIso*cos(FiIso)*
        (A + B*sinFiIso*sinFiIso + C*sinFiIso*sinFiIso*sinFiIso*sinFiIso +
         D*sinFiIso*sinFiIso*sinFiIso*sinFiIso*sinFiIso*sinFiIso);
      Lambda:=Meridian + deltaLambda;
    end
    else
    begin
      n:=avplattningSWEREF93/(2-avplattningSWEREF93);
      nKvadrat:=n*n;
      aTak:=(halvaStoraxelnSWEREF93/(1+n))*
              (1+(1/4)*nKvadrat+(1/64)*nKvadrat*nKvadrat);

      Ksi:=x/(medelMeridianFaktorSWEREF93*aTak);
      Aeta:=y/(medelMeridianFaktorSWEREF93*aTak);

      d1:=(1/2)*n-(2/3)*nKvadrat+(37/96)*n*nKvadrat-(1/360)*nKvadrat*nKvadrat;
      d2:=(1/48)*nKvadrat+(1/15)*n*nKvadrat-(437/1440)*nKvadrat*nKvadrat;
      d3:=(17/480)*n*nKvadrat-(37/840)*nKvadrat*nKvadrat;
      d4:=(4397/161280)*nKvadrat*nKvadrat;

      e2:=excentricitetKvadratenSWEREF93;
      e4:=e2*e2;
      e6:=e2*e4;
      e8:=e2*e6;
      A:=e2+e4+e6+e8;
      B:=-(1/6)*(7*e4+17*e6+30*e8);
      C:=(1/120)*(224*e6+889*e8);
      D:=-(1/1260)*(4279*e8);

      KsiPrim:=Ksi-
        d1*sin(2*Ksi)*cosh(2*Aeta)-
        d2*sin(4*Ksi)*cosh(4*Aeta)-
        d3*sin(6*Ksi)*cosh(6*Aeta)-
        d4*sin(8*Ksi)*cosh(8*Aeta);
      AetaPrim:=Aeta-
        d1*cos(2*Ksi)*sinh(2*Aeta)-
        d2*cos(4*Ksi)*sinh(4*Aeta)-
        d3*cos(6*Ksi)*sinh(6*Aeta)-
        d4*cos(8*Ksi)*sinh(8*Aeta);

      FiIso:=ArcSin(sin(KsiPrim)/cosh(AetaPrim));
      deltaLambdaIso:=ArcTan(sinh(AetaPrim)/cos(KsiPrim));
      deltaLambda:=deltaLambdaIso;

      sinFiIso:=sin(FiISo);
      Fi:=FiIso + sinFiIso*cos(FiIso)*
        (A + B*sinFiIso*sinFiIso + C*sinFiIso*sinFiIso*sinFiIso*sinFiIso +
         D*sinFiIso*sinFiIso*sinFiIso*sinFiIso*sinFiIso*sinFiIso);
      Lambda:=Meridian + deltaLambda;
    end;
    Fi:=RadToDeg(Fi);
    Lambda:=RadToDeg(Lambda);
  except
  end;
end;

procedure TWGS84.FiLambdaH2PlanaXY(Fi, Lambda, H : extended;
  var x, y : extended; system, medelMeridian : string);
var
  n, nKvadrat, aTak, Meridian : extended;
  AetaPrim, KsiPrim, deltaLambda : extended;
  FiIso, sinFi : extended;
  b1, b2, b3, b4, A, B, C, D, e2, e4, e6, e8 : extended;
begin
  if medelMeridian = '7.5 gon V' then
    Meridian:=Medelmeridianer[1].radianer
  else if medelMeridian = '5.0 gon V' then
    Meridian:=Medelmeridianer[2].radianer
  else if medelMeridian = '2.5 gon V' then
    Meridian:=Medelmeridianer[3].radianer
  else if medelMeridian = '0 gon' then
    Meridian:=Medelmeridianer[4].radianer
  else if medelMeridian = '2.5 gon O' then
    Meridian:=Medelmeridianer[5].radianer
  else if medelMeridian = '5.0 gon O' then
    Meridian:=Medelmeridianer[6].radianer;

  Fi:=DegToRad(Fi);
  Lambda:=DegToRad(Lambda);

  deltaLambda:=Lambda - Meridian;
  if system = 'Bessel' then
  begin
    sinFi:=sin(Fi);
    n:=avplattningBessel/(2-avplattningBessel);
    nKvadrat:=n*n;
    aTak:=(halvaStoraxelnBessel/(1+n))*
            (1+(1/4)*nKvadrat+(1/64)*nKvadrat*nKvadrat);

    e2:=excentricitetKvadratenBessel;
    e4:=e2*e2;
    e6:=e2*e4;
    e8:=e2*e6;
    A:=e2;
    B:=(1/6)*(5*e4-e6);
    C:=(1/120)*(104*e6-45*e8);
    D:=(1/1260)*(1237*e8);

    FiIso:=Fi - sinFi*cos(Fi)*
      (A + B*sinFi*sinFi + C*sinFi*sinFi*sinFi*sinFi +
       D*sinFi*sinFi*sinFi*sinFi*sinFi*sinFi);

    KsiPrim:=ArcTan(tan(FiIso)/cos(deltaLambda));
    AetaPrim:=ArcTanh(cos(FiIso)*sin(deltaLambda));

    b1:=(1/2)*n - (2/3)*nKvadrat + (5/16)*n*nKvadrat +
      (41/180)*nKvadrat*nKvadrat;
    b2:=(13/48)*nKvadrat - (3/5)*n*nKvadrat +
      (557/1440)*nKvadrat*nKvadrat;
    b3:=(61/240)*n*nKvadrat - (103/140)*nKvadrat*nKvadrat;
    b4:=(49561/161280)*nKvadrat*nKvadrat;

    x:= cosh(2*AetaPrim);
    x:=medelMeridianFaktorSWEREF93*aTak*
      (KsiPrim +
       b1*sin(2*KsiPrim)*cosh(2*AetaPrim) +
       b2*sin(4*KsiPrim)*cosh(4*AetaPrim) +
       b3*sin(6*KsiPrim)*cosh(6*AetaPrim) +
       b4*sin(8*KsiPrim)*cosh(8*AetaPrim));
    y:=medelMeridianFaktorSWEREF93*aTak*
      (AetaPrim +
       b1*cos(2*KsiPrim)*sinh(2*AetaPrim) +
       b2*cos(4*KsiPrim)*sinh(4*AetaPrim) +
       b3*cos(6*KsiPrim)*sinh(6*AetaPrim) +
       b4*cos(8*KsiPrim)*sinh(8*AetaPrim));
  end
  else
  begin
    sinFi:=sin(Fi);
    n:=avplattningSWEREF93/(2-avplattningSWEREF93);
    nKvadrat:=n*n;
    aTak:=(halvaStoraxelnSWEREF93/(1+n))*
            (1+(1/4)*nKvadrat+(1/64)*nKvadrat*nKvadrat);

    e2:=excentricitetKvadratenSWEREF93;
    e4:=e2*e2;
    e6:=e2*e4;
    e8:=e2*e6;
    A:=e2;
    B:=(1/6)*(5*e4-e6);
    C:=(1/120)*(104*e6-45*e8);
    D:=(1/1260)*(1237*e8);

    FiIso:=Fi - sinFi*cos(Fi)*
      (A + B*sinFi*sinFi + C*sinFi*sinFi*sinFi*sinFi +
       D*sinFi*sinFi*sinFi*sinFi*sinFi*sinFi);

    KsiPrim:=ArcTan(tan(FiIso)/cos(deltaLambda));
    AetaPrim:=ArcTanh(cos(FiIso)*sin(deltaLambda));

    b1:=(1/2)*n - (2/3)*nKvadrat + (5/16)*n*nKvadrat +
      (41/180)*nKvadrat*nKvadrat;
    b2:=(13/48)*nKvadrat - (3/5)*n*nKvadrat +
      (557/1440)*nKvadrat*nKvadrat;
    b3:=(61/240)*n*nKvadrat - (103/140)*nKvadrat*nKvadrat;
    b4:=(49561/161280)*nKvadrat*nKvadrat;

    x:=medelMeridianFaktorSWEREF93*aTak*
      (KsiPrim +
       b1*sin(2*KsiPrim)*cosh(2*AetaPrim) +
       b2*sin(4*KsiPrim)*cosh(4*AetaPrim) +
       b3*sin(6*KsiPrim)*cosh(6*AetaPrim) +
       b4*sin(8*KsiPrim)*cosh(8*AetaPrim));
    y:=medelMeridianFaktorSWEREF93*aTak*
      (AetaPrim +
       b1*cos(2*KsiPrim)*sinh(2*AetaPrim) +
       b2*cos(4*KsiPrim)*sinh(4*AetaPrim) +
       b3*cos(6*KsiPrim)*sinh(6*AetaPrim) +
       b4*cos(8*KsiPrim)*sinh(8*AetaPrim));
  end;
  y:=y+falseEastingSWEREF93;
end;

procedure TWGS84.PlanaXY2FiLambdaHSWEREF99(x, y : extended;
  var Fi, Lambda, H : extended; system : string);
var
  n, nKvadrat, aTak, Meridian, deltaLambda : extended;
  Aeta, Ksi, AetaPrim, KsiPrim : extended;
  FiIso, deltaLambdaIso, sinFiIso : extended;
  d1, d2, d3, d4, A, B, C, D, e2, e4, e6, e8 : extended;
  skalFaktor, medelMeridian, falseNorthing, falseEasting : extended;
  avplattning, halvaStoraxeln, excentricitetKvadraten : extended;
begin
  try
    H := 0;

    if system = 'RT90' then
    begin
      falseNorthing := falseNorthingRT90WGS84;
      falseEasting := falseEastingRT90WGS84;
      avplattning := avplattningSWEREF93;
      halvaStoraxeln := halvaStoraxelnSWEREF93;
      skalFaktor := medelMeridianFaktorRT90WGS84;
      medelMeridian := MedelmeridianRT90WGS84;
      excentricitetKvadraten := excentricitetKvadratenSWEREF93;
    end
    else if system = 'SWEREF99' then
    begin
      falseNorthing := falseNorthingSWEREF99;
      falseEasting := falseEastingSWEREF99;
      avplattning := avplattningSWEREF99;
      halvaStoraxeln := halvaStoraxelnSWEREF99;
      skalFaktor := medelMeridianFaktorSWEREF99;
      medelMeridian := MedelmeridianSWEREF99;
      excentricitetKvadraten := excentricitetKvadratenSWEREF99;
    end;

    x:=x-falseNorthing;
    y:=y-falseEasting;

    n:=avplattning/(2-avplattning);
    nKvadrat:=n*n;
    aTak:=(halvaStoraxeln/(1+n))*
            (1+(1/4)*nKvadrat+(1/64)*nKvadrat*nKvadrat);

    Ksi:=x/(skalFaktor*aTak);
    Aeta:=y/(skalFaktor*aTak);

    d1:=(1/2)*n-(2/3)*nKvadrat+(37/96)*n*nKvadrat-(1/360)*nKvadrat*nKvadrat;
    d2:=(1/48)*nKvadrat+(1/15)*n*nKvadrat-(437/1440)*nKvadrat*nKvadrat;
    d3:=(17/480)*n*nKvadrat-(37/840)*nKvadrat*nKvadrat;
    d4:=(4397/161280)*nKvadrat*nKvadrat;

    e2:=excentricitetKvadraten;
    e4:=e2*e2;
    e6:=e2*e4;
    e8:=e2*e6;
    A:=e2+e4+e6+e8;
    B:=-(1/6)*(7*e4+17*e6+30*e8);
    C:=(1/120)*(224*e6+889*e8);
    D:=-(1/1260)*(4279*e8);

    KsiPrim:=Ksi-
      d1*sin(2*Ksi)*cosh(2*Aeta)-
      d2*sin(4*Ksi)*cosh(4*Aeta)-
      d3*sin(6*Ksi)*cosh(6*Aeta)-
      d4*sin(8*Ksi)*cosh(8*Aeta);
    AetaPrim:=Aeta-
      d1*cos(2*Ksi)*sinh(2*Aeta)-
      d2*cos(4*Ksi)*sinh(4*Aeta)-
      d3*cos(6*Ksi)*sinh(6*Aeta)-
      d4*cos(8*Ksi)*sinh(8*Aeta);

    FiIso:=ArcSin(sin(KsiPrim)/cosh(AetaPrim));
    deltaLambdaIso:=ArcTan(sinh(AetaPrim)/cos(KsiPrim));
    deltaLambda:=deltaLambdaIso;

    sinFiIso:=sin(FiISo);
    Fi:=FiIso + sinFiIso*cos(FiIso)*
      (A + B*sinFiIso*sinFiIso + C*sinFiIso*sinFiIso*sinFiIso*sinFiIso +
       D*sinFiIso*sinFiIso*sinFiIso*sinFiIso*sinFiIso*sinFiIso);
    Lambda:=medelMeridian + deltaLambda;

    Fi:=RadToDeg(Fi);
    Lambda:=RadToDeg(Lambda);
  except
  end;
end;

procedure TWGS84.FiLambdaH2PlanaXYSWEREF99(Fi, Lambda, H : extended;
  var x, y : extended; system : string);
var
  n, nKvadrat, aTak, Meridian : extended;
  AetaPrim, KsiPrim, deltaLambda : extended;
  FiIso, sinFi : extended;
  b1, b2, b3, b4, A, B, C, D, e2, e4, e6, e8 : extended;
  skalFaktor, medelMeridian, falseNorthing, falseEasting : extended;
  avplattning, halvaStoraxeln, excentricitetKvadraten : extended;
begin
  try
    H := 0;

    if system = 'RT90' then
    begin
      falseNorthing := falseNorthingRT90WGS84;
      falseEasting := falseEastingRT90WGS84;
      avplattning := avplattningSWEREF93;
      halvaStoraxeln := halvaStoraxelnSWEREF93;
      skalFaktor := medelMeridianFaktorRT90WGS84;
      medelMeridian := MedelmeridianRT90WGS84;
      excentricitetKvadraten := excentricitetKvadratenSWEREF93;
    end
    else if system = 'SWEREF99' then
    begin
      falseNorthing := falseNorthingSWEREF99;
      falseEasting := falseEastingSWEREF99;
      avplattning := avplattningSWEREF99;
      halvaStoraxeln := halvaStoraxelnSWEREF99;
      skalFaktor := medelMeridianFaktorSWEREF99;
      medelMeridian := MedelmeridianSWEREF99;
      excentricitetKvadraten := excentricitetKvadratenSWEREF99;
    end;

    Fi:=DegToRad(Fi);
    Lambda:=DegToRad(Lambda);

    deltaLambda:=Lambda - medelMeridian;

    sinFi:=sin(Fi);
    n:=avplattning/(2-avplattning);
    nKvadrat:=n*n;
    aTak:=(halvaStoraxeln/(1+n))*
            (1+(1/4)*nKvadrat+(1/64)*nKvadrat*nKvadrat);

    e2:=excentricitetKvadraten;
    e4:=e2*e2;
    e6:=e2*e4;
    e8:=e2*e6;
    A:=e2;
    B:=(1/6)*(5*e4-e6);
    C:=(1/120)*(104*e6-45*e8);
    D:=(1/1260)*(1237*e8);

    FiIso:=Fi - sinFi*cos(Fi)*
      (A + B*sinFi*sinFi + C*sinFi*sinFi*sinFi*sinFi +
       D*sinFi*sinFi*sinFi*sinFi*sinFi*sinFi);

    KsiPrim:=ArcTan(tan(FiIso)/cos(deltaLambda));
    AetaPrim:=ArcTanh(cos(FiIso)*sin(deltaLambda));

    b1:=(1/2)*n - (2/3)*nKvadrat + (5/16)*n*nKvadrat +
      (41/180)*nKvadrat*nKvadrat;
    b2:=(13/48)*nKvadrat - (3/5)*n*nKvadrat +
      (557/1440)*nKvadrat*nKvadrat;
    b3:=(61/240)*n*nKvadrat - (103/140)*nKvadrat*nKvadrat;
    b4:=(49561/161280)*nKvadrat*nKvadrat;

    x:=skalFaktor*aTak*
      (KsiPrim +
       b1*sin(2*KsiPrim)*cosh(2*AetaPrim) +
       b2*sin(4*KsiPrim)*cosh(4*AetaPrim) +
       b3*sin(6*KsiPrim)*cosh(6*AetaPrim) +
       b4*sin(8*KsiPrim)*cosh(8*AetaPrim));
    y:=skalFaktor*aTak*
      (AetaPrim +
       b1*cos(2*KsiPrim)*sinh(2*AetaPrim) +
       b2*cos(4*KsiPrim)*sinh(4*AetaPrim) +
       b3*cos(6*KsiPrim)*sinh(6*AetaPrim) +
       b4*cos(8*KsiPrim)*sinh(8*AetaPrim));

    x:=x+falseNorthing;
    y:=y+falseEasting;
  except
  end;
end;

function TWGS84.fakultet(i : integer) : integer;
var
  temp : integer;
begin
  if i <= 0 then
    Result:=0
  else
  begin
    temp:=1;
    repeat
      temp:=temp*i;
      i:=i-1;
    until i = 0;
    Result:=temp;
  end;
end;

function TWGS84.GradMinutSekund2Radianer(Grad, Minut, Sekund : string) : extended;
var
  temp : extended;
begin
  Grad:=BytTillAktuellDecimalSeparator(Grad);
  Minut:=BytTillAktuellDecimalSeparator(Minut);
  Sekund:=BytTillAktuellDecimalSeparator(Sekund);
  temp:=StrToFloat(Grad) + StrToFloat(Minut)/60.0 + StrToFloat(Sekund)/3600.0;
  Result:=(ArcTan(1.0)/45.0)*temp;
end;

function TWGS84.BytTillAktuellDecimalSeparator(strangTal : string) : string;
var
  parr : array[0..255] of char;
  i : integer;
begin
  StrPCopy(parr,strangTal);
  for i:=1 to Length(strangTal) do
    if (parr[i-1] = '.') or (parr[i-1] = ',') then
      parr[i-1]:=DecimalSeparator;

  Result:=StrPas(parr);
end;

procedure TWGS84.SWEREF99ToWGS84AsDegreeAndDecimalMinute(x, y: extended;
  var Fi, Lambda, H: extended);
var
  minute : extended;
begin
  SWEREF99ToWGS84(x, y, Fi, Lambda, H);

  minute := Frac(Fi) * 60;
  minute := Round(minute * 10000) / 10000;
  Fi := Trunc(Fi) * 100 + minute;

  minute := Frac(Lambda) * 60;
  minute := Round(minute * 10000) / 10000;
  Lambda := Trunc(Lambda) * 100 + minute;
end;

procedure TWGS84.WGS84AsDegreeAndDecimalMinuteToSWEREF99(Fi, Lambda,
  H: extended; var x, y: extended);
var
  minute : extended;
  strMinute : string;
begin
  strMinute := FloatToStr(Fi);
  strMinute := Copy(strMinute, 3, Length(strMinute) - 2);
  minute := StrToFloat(strMinute);
  Fi := Trunc(Fi / 100);
  Fi := Fi + minute / 60;

  strMinute := FloatToStr(Lambda);
  strMinute := Copy(strMinute, 3, Length(strMinute) - 2);
  minute := StrToFloat(strMinute);
  Lambda := Trunc(Lambda / 100);
  Lambda := Lambda + minute / 60;

  WGS84ToSWEREF99(Fi, Lambda, H, x, y);
end;

// UTM -------------------------------------------------------------------------

// Ellipsoid model IAG GRS 1980 constants (actual values here are for WGS84)

Const  ct_dga = 6378137;      //demi grand axe
      ct_dpa = 6356752.314;  //demi petit axe
      ct_UTMScaleFactor = 0.9996; //facteur d'échelle

{   * ArcLengthOfMeridian
   *
   * Computes the ellipsoidal distance from the equator to a point at a
   * given latitude.
   *
   * Reference: Hoffmann-Wellenhof, B., Lichtenegger, H., and Collins, J.,
   * GPS: Theory and Practice, 3rd ed.  New York: Springer-Verlag Wien, 1994.
   *
   * Inputs:
   *     phi - Latitude of the point, in radians.
   *
   * Globals:
   *     ct_dga - Ellipsoid model major axis.
   *     ct_dpa - Ellipsoid model minor axis.
   *
   * Returns:
   *     The ellipsoidal distance of the point from the equator, in meters.
}
function ArcLengthOfMeridian(phi:Double):Double;

var alpha, beta, gamma, delta, epsilon, n: Double;
begin
  //Precalculate n
  n := (ct_dga - ct_dpa) / (ct_dga + ct_dpa);
  //Precalculate alpha
  alpha := ((ct_dga + ct_dpa) / 2)*(1 + (intPower (n, 2) / 4) + (intPower (n, 4) / 64));
  //Precalculate beta
  beta := (-3 * n / 2) + (9 * intPower (n, 3) / 16) + (-3 * intPower (n, 5) / 32);
  //Precalculate gamma
  gamma := (15 * intPower (n, 2) / 16) + (-15 * intPower (n, 4) / 32);
  //Precalculate delta
  delta := (-35 * intPower (n, 3) / 48) + (105 * intPower (n, 5) / 256);
  //Precalculate epsilon
  epsilon := (315 * intPower (n, 4) / 512);
  //Now calculate the sum of the series and return
  result := alpha * (phi + (beta * sin (2 * phi)) + (gamma * sin (4 * phi))
            + (delta * sin (6 * phi)) + (epsilon * sin (8 * phi)));
end;

{   * UTMCentralMeridian
    *
    * Determines the central meridian for the given UTM fuseau.
    *
    * Inputs:
    *     fuseau - An integer value designating the UTM fuseau, range [1,60].
    *
    * Returns:
    *   The central meridian for the given UTM fuseau, in radians, or zero
    *   if the UTM fuseau parameter is outside the range [1,60].
    *   Range of the central meridian is the radian equivalent of [-177,+177].
}
function UTMCentralMeridian(fuseau:integer):Double;
begin
  Result := DegToRad (-183 + (fuseau * 6));
end;

{   * FootpointLatitude
    *
    * Computes the footpoint latitude for use in converting transverse
    * Mercator coordinates to ellipsoidal coordinates.
    *
    * Reference: Hoffmann-Wellenhof, B., Lichtenegger, H., and Collins, J.,
    *   GPS: Theory and Practice, 3rd ed.  New York: Springer-Verlag Wien, 1994.
    *
    * Inputs:
    *   y - The UTM northing coordinate, in meters.
    *
    * Returns:
    *   The footpoint latitude, in radians.
}
function FootpointLatitude (y:Double):Double;
var y_, alpha_, beta_, gamma_, delta_, epsilon_, n:Double;
begin
  // Precalculate n (Eq. 10.18)
  n := (ct_dga - ct_dpa) / (ct_dga + ct_dpa);
  //Precalculate alpha_ (Eq. 10.22) (Same as alpha in Eq. 10.17)
  alpha_ := ((ct_dga + ct_dpa) / 2)
         * (1 + (intPower (n, 2) / 4) + (intPower (n, 4) / 64));
  //Precalculate y_ (Eq. 10.23)
  y_ := y / alpha_;
  //Precalculate beta_ (Eq. 10.22)
  beta_ := (3 * n / 2) + (-27 * intPower (n, 3) / 32) + (269 * intPower (n, 5) / 512);
  //Precalculate gamma_ (Eq. 10.22)
  gamma_ := (21 * intPower (n, 2) / 16) + (-55 * intPower (n, 4) / 32);
  //Precalculate delta_ (Eq. 10.22)
  delta_ := (151 * intPower (n, 3) / 96) + (-417 * intPower (n, 5) / 128);
  //Precalculate epsilon_ (Eq. 10.22)
  epsilon_ := (1097 * intPower (n, 4) / 512);
  //Now calculate the sum of the series (Eq. 10.21)
  result := y_ + (beta_ * sin (2 * y_)) + (gamma_ * sin (4 * y_))
         + (delta_ * sin (6 * y_)) + (epsilon_ * sin (8 * y_));
end;

{   * MapLatLonToXY
    *
    * Converts a latitude/longitude pair to x and y coordinates in the
    * Transverse Mercator projection.  Note that Transverse Mercator is not
    * the same as UTM; a scale factor is required to convert between them.
    *
    * Reference: Hoffmann-Wellenhof, B., Lichtenegger, H., and Collins, J.,
    * GPS: Theory and Practice, 3rd ed.  New York: Springer-Verlag Wien, 1994.
    *
    * Inputs:
    *    phi - Latitude of the point, in radians.
    *    lambda - Longitude of the point, in radians.
    *    lambda0 - Longitude of the central meridian to be used, in radians.
    *
    * Outputs:
    *    UTM - A 2-element containing the UTM.x and UTM.y coordinates
    *         of the computed point.
    *
}

procedure MapLatLonToXY (LatLon:TrecLatLon; lambda0:Double; var UTM:TrecUTM);
var N, nu2, ep2, t, t2, l, phi, lambda:Double;
var l3coef, l4coef, l5coef, l6coef, l7coef, l8coef:Double;
begin
  phi:=LatLon.Lat; lambda:=LatLon.Lon;
  // Precalculate ep2
  ep2 := (intPower (ct_dga, 2) - intPower (ct_dpa, 2)) / intPower (ct_dpa, 2);
  // Precalculate nu2
  nu2 := ep2 * intPower (cos (phi), 2);
  // Precalculate N
  N := intPower (ct_dga, 2) / (ct_dpa * sqrt (1 + nu2));
  // Precalculate t
  t := tan (phi); t2 := t * t;
  // Precalculate l
  l := lambda - lambda0;
  // Precalculate coefficients for l**n in the equations below
  // so a normal human being can read the expressions for easting and northing
  // -- l**1 and l**2 have coefficients of 1
  l3coef := 1 - t2 + nu2; l4coef := 5 - t2 + 9 * nu2 + 4 * (nu2 * nu2);
  l5coef := 5 - 18 * t2 + (t2 * t2) + 14 * nu2 - 58 * t2 * nu2;
  l6coef := 61 - 58 * t2 + (t2 * t2) + 270 * nu2 - 330 * t2 * nu2;
  l7coef := 61 - 479 * t2 + 179 * (t2 * t2) - (t2 * t2 * t2);
  l8coef := 1385 - 3111 * t2 + 543 * (t2 * t2) - (t2 * t2 * t2);
  // Calculate easting (x)
  UTM.X := N * cos (phi) * l
        + (N / 6 * intPower (cos (phi), 3) * l3coef * intPower (l, 3))
        + (N / 120 * intPower (cos (phi), 5) * l5coef * intPower (l, 5))
        + (N / 5040 * intPower (cos (phi), 7) * l7coef * intPower (l, 7));

  // Calculate northing (y)
  UTM.Y := ArcLengthOfMeridian (phi)
        + (t / 2 * N * intPower (cos (phi), 2) * intPower (l, 2))
        + (t / 24 * N * intPower (cos (phi), 4) * l4coef * intPower (l, 4))
        + (t / 720 * N * intPower (cos (phi), 6) * l6coef * intPower (l, 6))
        + (t / 40320 * N * intPower (cos (phi), 8) * l8coef * intPower (l, 8));
end;

{   * MapXYToLatLon
    *
    * Converts x and y coordinates in the Transverse Mercator projection to
    * a latitude/longitude pair.  Note that Transverse Mercator is not
    * the same as UTM; a scale factor is required to convert between them.
    *
    * Reference: Hoffmann-Wellenhof, B., Lichtenegger, H., and Collins, J.,
    *   GPS: Theory and Practice, 3rd ed.  New York: Springer-Verlag Wien, 1994.
    *
    * Inputs:
    *   UTM.x - The easting of the point, in meters.
    *   UTM.y - The northing of the point, in meters.
    *   lambda0 - Longitude of the central meridian to be used, in radians.
    *
    * Outputs:
    *   philambda - A 2-element containing the latitude and longitude
    *               in radians.
    *
    *
    * Remarks:
    *   The local variables Nf, nuf2, tf, and tf2 serve the same purpose as
    *   N, nu2, t, and t2 in MapLatLonToXY, but they are computed with respect
    *   to the footpoint latitude phif.
    *
    *   x1frac, x2frac, x2poly, x3poly, etc. are to enhance readability and
    *   to optimize computations.
}

procedure MapXYToLatLon (UTM:TrecUTM; lambda0:Double; var philambda:TrecLatLon);
var phif, Nf, Nfpow, nuf2, ep2, tf, tf2, tf4, cf,x ,y:Double;
var x1frac, x2frac, x3frac, x4frac, x5frac, x6frac, x7frac, x8frac:Double;
var x2poly, x3poly, x4poly, x5poly, x6poly, x7poly, x8poly:Double;
begin
  x:=UTM.X; y:=UTM.Y;
  // Get the value of phif, the footpoint latitude.
  phif := FootpointLatitude (y);
  // Precalculate ep2
  ep2 := (intPower (ct_dga, 2) - intPower (ct_dpa, 2)) / intPower (ct_dpa, 2);
  // Precalculate cos (phif)
  cf := cos (phif);
  // Precalculate nuf2
  nuf2 := ep2 * intPower (cf, 2);
  // Precalculate Nf and initialize Nfpow
  Nf := intPower (ct_dga, 2) / (ct_dpa * sqrt (1 + nuf2)); Nfpow := Nf;
  // Precalculate tf
  tf := tan (phif); tf2 := tf * tf; tf4 := tf2 * tf2;
  // Precalculate fractional coefficients for x**n in the equations
  //below to simplify the expressions for latitude and longitude.
  x1frac := 1 / (Nfpow * cf);
  Nfpow := Nfpow * Nf; x2frac := tf / (2 * Nfpow);  // now equals Nf**2)
  Nfpow := Nfpow * Nf; x3frac := 1 / (6 * Nfpow * cf);  // now equals Nf**3)
  Nfpow := Nfpow * Nf; x4frac := tf / (24 * Nfpow);  // now equals Nf**4)
  Nfpow := Nfpow * Nf; x5frac := 1 / (120 * Nfpow * cf);  // now equals Nf**5)
  Nfpow := Nfpow * Nf; x6frac := tf / (720 * Nfpow);  // now equals Nf**6)
  Nfpow := Nfpow * Nf; x7frac := 1 / (5040 * Nfpow * cf);  // now equals Nf**7)
  Nfpow := Nfpow * Nf; x8frac := tf / (40320 * Nfpow);  // now equals Nf**8)
  // Precalculate polynomial coefficients for x**n.
  // -- x**1 does not have a polynomial coefficient.
  x2poly := -1 - nuf2; x3poly := -1 - 2 * tf2 - nuf2;
  x4poly := 5 + 3 * tf2 + 6 * nuf2 - 6 * tf2 * nuf2
         - 3 * (nuf2 *nuf2) - 9 * tf2 * (nuf2 * nuf2);
  x5poly := 5 + 28 * tf2 + 24 * tf4 + 6 * nuf2 + 8 * tf2 * nuf2;
  x6poly := -61 - 90 * tf2 - 45 * tf4 - 107 * nuf2 + 162 * tf2 * nuf2;
  x7poly := -61 - 662 * tf2 - 1320 * tf4 - 720 * (tf4 * tf2);
  x8poly := 1385 + 3633 * tf2 + 4095 * tf4 + 1575 * (tf4 * tf2);

  // Calculate latitude
  philambda.Lat := phif + x2frac * x2poly * (x * x) + x4frac * x4poly * intPower (x, 4)
        	+ x6frac * x6poly * intPower (x, 6) + x8frac * x8poly * intPower (x, 8);

  // Calculate longitude
  philambda.Lon := lambda0 + x1frac * x + x3frac * x3poly * intPower (x, 3)
        	+ x5frac * x5poly * intPower (x, 5) + x7frac * x7poly * intPower (x, 7);
end;

 {  * LatLonToUTMXY
    *
    * Converts a latitude/longitude pair to x and y coordinates in the
    * Universal Transverse Mercator projection.
    *
    * Inputs:
    *   lat - Latitude of the point, in radians.
    *   lon - Longitude of the point, in radians.
    *   fuseau - UTM fuseau to be used for calculating values for x and y.
    *          If fuseau is less than 1 or greater than 60, the routine
    *          will determine the appropriate fuseau from the value of lon.
    *
    * Outputs:
    *   UTM - A 2-element where the UTM.x and UTM.y values will be stored.
    *
    * Returns:
    *   The UTM.fuseau used for calculating the values of x and y.
}

function LatLonToUTMXY (LatLon:TrecLatLon; var UTM:TrecUTM):Integer;
begin
  MapLatLonToXY (LatLon, UTMCentralMeridian (UTM.fuseau), UTM); //calcul xy
  // Adjust easting and northing for UTM system.
  UTM.X := UTM.X * ct_UTMScaleFactor + 500000;
  UTM.Y := UTM.Y * ct_UTMScaleFactor;
  if UTM.Y < 0 then UTM.Y := UTM.Y + 10000000;
  Result:=UTM.fuseau;
end;

{   * UTMXYToLatLon
    *
    * Converts x and y coordinates in the Universal Transverse Mercator
    * projection to a latitude/longitude pair.
    *
    * Inputs:
    *	UTM.x - The easting of the point, in meters.
    *	UTM.y - The northing of the point, in meters.
    *	UTM.fuseau - The UTM fuseau in which the point lies.
    *	UTM.southhemi - True if the point is in the southern hemisphere;
    *               false otherwise.
    *
    * Outputs:
    *	latlon - A 2-element containing the latitude and
    *            longitude of the point, in radians.
    *
}
procedure UTMXYToLatLon (UTM:TrecUTM; var latlon:TrecLatLon);
var cmeridian:Double;
begin
  UTM.x := UTM.x - 500000; UTM.x := UTM.x / ct_UTMScaleFactor;
  // If in southern hemisphere, adjust y accordingly.
  if UTM.southhemi then UTM.y := UTM.y - 10000000;
  UTM.y := UTM.y / ct_UTMScaleFactor;
  cmeridian := UTMCentralMeridian (UTM.fuseau);
  MapXYToLatLon (UTM, cmeridian, latlon);
end;

function Limits(LatLon:TrecLatLon):boolean;
{   * Limits N Lat = 84°
    * Limits S Lat = -80°
    * Lon = +180 -180
}
begin
  Result:=((LatLon.Lat <= 84) and (LatLon.Lat >= -80)) and
  ((LatLon.Lon <= 180) and (LatLon.Lon >= -180));
end;

function CharLat(LatLon:TrecLatLon):char;
{   * N = 0°-> 8 , P = 8° -> 16°, Q = 16° -> 24°
    * R = 24° -> 32° , S = 32° -> 40° , T = 40° -> 48°
    * U = 48° -> 56° , V = 56° -> 64° , W = 64° -> 72°
    * X = 72° -> 84°.
    * C = -80° -> -72° , D = -72° -> -64° , E = -64° -> -56°
    * F = -56° -> -48° , G = -48° -> -40° , H = -40° -> -32°
    * J = -32° -> -24° , K = -24° -> -16° , L = -16° -> -8°
    * M = -8° -> 0.
}
var Od : integer;
begin
  Od:=floor(abs(LatLon.Lat/8));
  if LatLon.Lat < 0 then
  begin
    Od:=9-Od; if Od < 0 then Od:=0;
    Result:=chr(ord('C')+Od);
    if Result > 'H' then inc(Result);
  end else
  begin
    if Od > 9 then Od:=9;
    Result:=chr(ord('N')+Od); if Result > 'N' then inc(Result);
  end;
end;

//eof sp------------------------------------------------------------------
{  * LatLon_TO_UTM
     Please enter a valid longitude in the lon field
     Please enter a number in the range [-180, 180)
     Please enter a valid latitude in the lat field
     Please enter a number in the range [-80, 84]
     not UTM.OK = bad
}

procedure TWGS84.WGS84ToUTM(LatLon:TrecLatLon; var UTM:TrecUTM);
var           // for reversal of UTM-X and UTM-Y to geodetic orientation
  t : double; // för omkastning av UTM-X och UTM-Y till geodetisk orientering
begin
  UTM.OK:=Limits(LatLon); if not UTM.OK then exit;

  UTM.CharLat:=CharLat(LatLon);
  UTM.fuseau := floor ((LatLon.lon + 180) / 6) + 1;
  LatLon.Lat:=DegToRad (LatLon.Lat); LatLon.Lon:=DegToRad (LatLon.Lon);
  UTM.fuseau := LatLonToUTMXY (LatLon,UTM);
  UTM.southhemi:=LatLon.Lat < 0;

  if not FIgnoreGeodeticOrientation then
  begin
    if UTM.southhemi then UTM.Y:=UTM.Y*-1;

    // reversal of UTM-X and UTM-Y to geodetic orientation
    // kasta om UTM-X och UTM-Y till geodetisk orientering
    t := UTM.X;
    UTM.X := UTM.Y;
    UTM.Y := t;
  end;
end;

{   UTM_TO_LatLon
    Please enter a valid easting in the x field
    Please enter a valid northing in the y field (-y for southhemi)
    Please enter a valid UTM fuseau in the fuseau field
    Please enter a number in the range [1, 60]
    southhemi = northing < 0
    not LatLon.OK = bad
}

procedure TWGS84.UTMToWGS84(UTM:TrecUTM; var latlon:TrecLatLon);
var
  t : double; // för omkastning av UTM-X och UTM-Y från geodetisk orientering
  LonH,LonL:Integer;
begin
  LatLon.OK:=(UTM.fuseau >= 1) and (UTM.fuseau <= 60);
  if not LatLon.OK then exit;

  if not FIgnoreGeodeticOrientation then
  begin
    // kasta om UTM-X och UTM-Y från geodetisk orientering
    t := UTM.X;
    UTM.X := UTM.Y;
    UTM.Y := t;

    UTM.southhemi:=UTM.Y < 0; if UTM.southhemi then UTM.Y:=abs(UTM.Y);
  end;

  UTMXYToLatLon (UTM,latlon);
  latlon.Lat:=RadToDeg(LatLon.Lat); latLon.Lon:=RadToDeg(LatLon.Lon);
  if UTM.southhemi then UTM.Y:=UTM.Y*-1;

  LatLon.OK:=Limits(LatLon); if not LatLon.OK then exit;
  LonL:=-186 + (UTM.fuseau * 6); LonH:=LonL+6;
  LatLon.OK:=(LatLon.Lon >= LonL) and (LatLon.lon <= LonH);
end;


end.
