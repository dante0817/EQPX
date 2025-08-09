{
ZWECK       : Peilwinkel und Abstand zwischen zwei geograph. Koordinatenpunkten

DATEIEN     : Bearing.pas
              Test_Bearing.pas
              Test_Bearing_MainForm.pas
              Test_Bearing_MainForm.dfm

ÄNDERUNGEN  : Jan.99  Version 1.0

AUTOR       : Kurt Spitzley, email Kurt.Spitzley@rz-online.de oder
                                   101.10490@germanynet.de

COPYRIGHT   : (c) 1998 bei Kurt Spitzley
              Diese Software darf zu nichtkommerziellen Zwecken frei genutzt
              und weitergegeben werden.
GEWÄHR-
LEISTUNG    : Sie akzeptieren diese Software wie sie ist, ohne eine Garantie
              jeglicher Art, einschließlich aber nicht ausschließlich der
              Eignung für eine beliebige Anwendung.

BESCHREIBUNG: s.o.

================================================================================

SCOPE       : Bearing and Distance between two geographical coordinates

FILES       : Bearing.pas
              Test_Bearing.pas
              Test_Bearing_MainForm.pas
              Test_Bearing_MainForm.dfm

LAST CHANGES: V1.0 Jan.99

AUTHOR      : Kurt Spitzley, email Kurt.Spitzley@rz-online.de or
                                   101.10490@germanynet.de

COPYRIGHT   : (c) 1998 by Kurt Spitzley, All rights reserved.
              This software should not be SOLD by anyone. It is distributed as
              freeware and therefore may be used free of charge.

DISCLAIMER  : You accept this software AS IS without any representation or
              warranty of any kind, including but not limited to the warranty of
              merchantability or fitness for a particular purpose.

DESCRIPTION:  class to calculate distance and bearing between two points on the
              WGS84 ellipsoid
              Original C++ code by P.Luthaus
              Converted to Object Pascal and extended by Kurt Spitzley
              Ref.doc. see RTCA/DO-208, Change 1, Appendix B;
              T. Vincenty, Survey Review No 176 (1975), pp. 88-93
}

unit Bearing;

interface

uses Math;

type
  TDegMinSec = record
    Degrees: integer;
    Minutes: byte;
    Seconds: Extended;
  end;

  TLaLoDegPoint= record
    Latitude,
    Longitude: Extended;
  end;

  TBearingData = record
    Bearing,
    Distance: Extended;
  end;

  TBearing = class
  private
    FDistance, //distance
    FInitialBearing, //bearing at point of departure
    FFinalBearing, //bearing at point of destination
    Lat1,
    Lat2,
    dLon,
    Beta1,
    Beta2,
    Lambda,
    CosSqrAlpha,
    Lambda0,
    SinSigma,
    Cos2Sigmam,
    CosSqr2Sigmam,
    CosSigma,
    Sigma,
    SinBeta1,
    SinBeta2,
    CosBeta1,
    CosBeta2,
    CosLambda0,
    SinLambda0,
    SinAlpha,
    SinLambda,
    CosLambda,
    uSqr,
    dSigma,A1,B1,C1: Extended;
    Iter: integer;
  public
    function Bearing(p1,p2:TLaLoDegPoint): TBearingData;
    procedure Calculate(p1,p2:TLaLoDegPoint);
    property InitialBearing:extended read FInitialBearing;
    property FinalBearing:extended read FFinalBearing;
    property Distance:extended read FDistance;
  end;

implementation

//TBearing---------------------------------------------------------------------

const
  a: Extended         = 6378137.0;        //WGS84 semimajor axis
  b: Extended         = 6356752.3142;     //WGS84 semiminor axis
  e_Sqr: Extended     = 6.694379991013e-3;//square of WGS84 1st eccentricity
  ep_Sqr: Extended    = 6.73949674227e-3; //square of WGS84 2nd eccentricity
  f: Extended         = 3.35281066474e-3; //WGS84 flattening
  epsilon: Extended   = 1e-12;           //termination criterion
  MinNumber: Extended = 1e-50;           //smallest number > 0
  MaxIter:integer     = 100;
  DegToRad: Extended  = PI/180;
  RadToDeg: Extended  = 180 / PI;

function fmod(x,y: Extended):Extended;
begin
  Result:=x-(int(x / y) * y);
end;

procedure TBearing.Calculate(p1,p2:TLaLoDegPoint);
begin
  try
  Lat1 := p1.Latitude * DegToRad; // convert latitudes to radians
  Lat2 := p2.Latitude * DegToRad;

  dLon := (p2.Longitude - p1.Longitude) * DegToRad; // calc. diff. of longitudes
  Beta1 := ArcTan((1-f)* Tan(Lat1)); // calculate 'reduced latitudes'
  Beta2 := ArcTan((1-f)* Tan(Lat2));
  Lambda := dLon;

  Iter:=0;
  CosBeta1 := Cos(Beta1)+1e-15;
  SinBeta1 := Sin(Beta1)+1e-15;
  CosBeta2 := Cos(Beta2);
  SinBeta2 := Sin(Beta2);

  repeat
    inc(Iter);

    Lambda0 := Lambda;
    CosLambda0 := Cos(Lambda0);
    SinLambda0 := Sin(Lambda0);

    SinSigma := Sqrt(Sqr(CosBeta2 * SinLambda0) + Sqr(CosBeta1 * SinBeta2 -
                SinBeta1 * CosBeta2 * CosLambda0) );
    CosSigma := SinBeta1 * SinBeta2 + CosBeta1 * CosBeta2 * CosLambda0;

    Sigma := ArcTan2(SinSigma , CosSigma);
    SinAlpha := (CosBeta1 * CosBeta2 * SinLambda0) / SinSigma;
    CosSqrAlpha := (1 + SinAlpha) * (1 - SinAlpha);

    if abs(CosSqrAlpha) < MinNumber then
      Cos2Sigmam := 0
    else
      Cos2Sigmam := CosSigma - ((2 * SinBeta1 * SinBeta2) / CosSqrAlpha);

    C1 := (f / 16) * CosSqrAlpha * (4 + f * (4 - 3 * CosSqrAlpha));
    CosSqr2Sigmam := Sqr(Cos2Sigmam);
    Lambda := dLon + (1-C1) * f * SinAlpha * (Sigma + C1 * SinSigma * (Cos2Sigmam
              +C1 * CosSigma * (-1 + 2 * CosSqr2Sigmam)));
  until not((Iter < MaxIter) and (abs(Lambda - Lambda0)>epsilon));

  uSqr := ep_Sqr * CosSqrAlpha;
  A1 :=1 + (uSqr / 16384) * (4096 + uSqr * (-768 + uSqr * (320 -175 * uSqr)));
  B1 := (uSqr / 1024) * (256 + uSqr * (-128 + uSqr * (74 - 47 * uSqr)));
  dSigma := B1 * SinSigma * (Cos2Sigmam + (B1 / 4) * ((-1 + 2 * CosSqr2Sigmam) * CosSigma -
            (B1 / 6) * (-3 + 4 * Sqr(SinSigma)) * (-3 + 4 * CosSqr2Sigmam) * Cos2Sigmam));

  FDistance := b * A1 * (Sigma - dSigma); // distance

  SinLambda := Sin(Lambda);
  CosLambda := Cos(Lambda);

  // initial bearing
  FInitialBearing := ArcTan2(CosBeta2 * SinLambda,
              (CosBeta1 * SinBeta2 - SinBeta1 * CosBeta2 * CosLambda));

  FInitialBearing := RadToDeg * fmod (FInitialBearing + 2 * PI, 2 * PI);

  // final bearing
  FFinalBearing := ArcTan2(CosBeta1 * SinLambda,
              (CosBeta1 * SinBeta2 * CosLambda - SinBeta1 * CosBeta2));
  FFinalBearing := RadToDeg * fmod(FFinalBearing + 2 * PI, 2 * PI);
  except
  end;
end;

function TBearing.Bearing(p1,p2:TLaLoDegPoint): TBearingData;
begin
  Calculate(p1,p2);
  Result.Distance:=Distance;
  Result.Bearing:=FinalBearing;
end;

end.



