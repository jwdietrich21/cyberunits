unit SignalAnalysis;

{ CyberUnits }

{ Object Pascal units for computational cybernetics }

{ Bricks: Basic blocks for information processing structures }

{ Version 1.1.0 (Director) }

{ (c) Johannes W. Dietrich, 1994 - 2015 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2015 }

{ Standard signal processing tools }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://cyberunits.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  maxarraysize = 128;
  halfmaxsize  = 64;
  maxfreqsize  = 63;

type
  dataindextype = 1 .. maxarraysize;
  cmpxindextype = 1 .. halfmaxsize;
  freqindextype = 1 .. maxfreqsize;

  complex = record
    realpart, imagpart: real
  end;

  dataarraytype = record
    case boolean of
    false: (rp: array [dataindextype] of real);
    true: (cp: array [cmpxindextype] of complex)
  end;

  cstermtype = record
    cosineterm, sineterm: real
  end;

  fouriertype = record
    dcterm:    real;
    noiseterm: real;
    freqterms: array [freqindextype] of cstermtype
  end;

  mixedtype = record
    case boolean of
    false: (dataslot: dataarraytype);
    true: (coefslot: fouriertype);
  end;

const
  twopi = 2 * pi;

procedure fftofreal(var mixed: mixedtype; realpoints: integer);
function omegat(f: freqindextype; t: dataindextype): real;

implementation

procedure fftofreal(var mixed: mixedtype; realpoints: integer);
{ Adaptation of Bob Schor's algorithm }

 var
  index, minusindex: freqindextype;
  temp1, temp2, temp3, w: complex;
  baseangle: real;

  procedure cadd(a, b: complex; var c: complex);

  { c := a + b }

  begin   { cadd }
    with c do
    begin
      realpart := a.realpart + b.realpart;
      imagpart := a.imagpart + b.imagpart;
    end;
  end;

  procedure csubtract(a, b: complex; var c: complex);

  { c := a - b }

  begin   { csubtract }
    with c do
    begin
      realpart := a.realpart - b.realpart;
      imagpart := a.imagpart - b.imagpart;
    end;
  end;

  procedure cmultiply(a, b: complex; var c: complex);

  { c := a * b }

  begin   { cmultiply }
    with c do
    begin
      realpart := a.realpart * b.realpart - a.imagpart * b.imagpart;
      imagpart := a.realpart * b.imagpart + b.realpart * a.imagpart;
    end;
  end;

  procedure conjugate(a: complex; var b: complex);

  { b := a* }

  begin   { conjugate }
    with b do
    begin
      realpart := a.realpart;
      imagpart := -a.imagpart;
    end;
  end;

  procedure forwardfft(var Data: dataarraytype;
    complexpoints: integer);
  const
    twopi = 6.2831853;

    procedure docomplextransform;

    var
      partitionsize, halfsize, offset, lowindex, highindex: dataindextype;
      baseangle, angle: real;
      bits:    integer;
      w, temp: complex;

    begin   { docomplextransform }
      partitionsize := complexpoints;
      with Data do
        repeat
          halfsize  := partitionsize div 2;
          baseangle := twopi / partitionsize;
          for offset := 1 to halfsize do
          begin
            angle      := baseangle * pred(offset);
            w.realpart := cos(angle);
            w.imagpart := -sin(angle);
            lowindex   := offset;
            repeat
              highindex := lowindex + halfsize;
              csubtract(cp[lowindex], cp[highindex], temp);
              cadd(cp[lowindex], cp[highindex], cp[lowindex]);
              cmultiply(temp, w, cp[highindex]);
              lowindex := lowindex + partitionsize
            until lowindex >= complexpoints;
          end;
          partitionsize := partitionsize div 2
        until partitionsize = 1;
    end;

    procedure shufflecoefficients;

    var
      lowindex, highindex: dataindextype;
      bits: integer;

      function log2(index: integer): integer;

        { Recursive routine, where "index" is assumed a power of 2.
         Note the routine will fail (by endless recursion) if
         "index" <= 0. }

      begin   { log2 }
        if index = 1 then
          log2 := 0
        else
          log2 := succ(log2(index div 2));
      end;

      function bitreversal(index, bits: integer): integer;

        { Takes an index, in the range 1 .. 2**bits, and computes a
         bit-reversed index in the same range.  It first undoes the
         offset of 1, bit-reverses the "bits"-bit binary number,
         then redoes the offset.  Thus if bits = 4, the range is
         1 .. 16, and bitreversal (1, 4) = 9,
         bitreversal (16, 4) = 16, etc. }

        function reverse(bits, stib, bitsleft: integer): integer;

          { Recursive bit-reversing function, transforms "bits" into
           bit-reversed "stib.  It's pretty easy to convert this to
           an iterative form, but I think the recursive form is
           easier to understand, and should entail a trivial penalty
           in speed (in the overall algorithm). }

        begin   { reverse }
          if bitsleft = 0 then
            reverse := stib
          else
          if odd(bits) then
            reverse := reverse(bits div 2, succ(stib * 2),
              pred(bitsleft))
          else
            reverse := reverse(bits div 2, stib * 2,
              pred(bitsleft));
        end;

      begin   { bitreversal }
        bitreversal := succ(reverse(pred(index), 0, bits));
      end;

      procedure swap(var a, b: complex);

      var
        temp: complex;

      begin   { swap }
        temp := a;
        a    := b;
        b    := temp;
      end;

    begin   { shufflecoefficients }
      bits := log2(complexpoints);
      with Data do
        for lowindex := 1 to complexpoints do
        begin
          highindex := bitreversal(lowindex, bits);
          if highindex > lowindex then
            swap(cp[lowindex], cp[highindex]);
        end;
    end;

    procedure dividebyn;

    { This procedure is needed to get FFT to scale correctly. }

    var
      index: dataindextype;

    begin   { dividebyn }
      with Data do
        for index := 1 to complexpoints do
          with cp[index] do
          begin
            realpart := realpart / complexpoints;
            imagpart := imagpart / complexpoints;

          end;
    end;

  begin   { forwardfft }
    docomplextransform;
    shufflecoefficients;
    dividebyn;
  end;

begin   { fftofreal }
  forwardfft(mixed.dataslot, realpoints div 2);
  temp1 := mixed.dataslot.cp[1];
  with mixed.coefslot, temp1 do
  begin
    dcterm    := (realpart + imagpart) / 2;
    noiseterm := (realpart - imagpart) / 2;
  end;
  baseangle := -twopi / realpoints;
  for index := 1 to realpoints div 4 do
  begin
    minusindex := (realpoints div 2) - index;
    with mixed.dataslot do
    begin
      conjugate(cp[succ(minusindex)], temp2);
      cadd(cp[succ(index)], temp2, temp1);
      csubtract(cp[succ(index)], temp2, temp2);
    end;
    w.realpart := sin(index * baseangle);
    w.imagpart := -cos(index * baseangle);
    cmultiply(w, temp2, temp2);
    cadd(temp1, temp2, temp3);
    csubtract(temp1, temp2, temp2);
    conjugate(temp2, temp2);
    with mixed.coefslot.freqterms[index], temp3 do
    begin
      cosineterm := realpart / 2;
      sineterm   := -imagpart / 2;
    end;
    with mixed.coefslot.freqterms[minusindex], temp2 do
    begin
      cosineterm := realpart / 2;
      sineterm   := imagpart / 2;
    end;
  end;
end;

function omegat(f: freqindextype; t: dataindextype): real;

  { computes omega*t for particular harmonic, index }

begin   { omegat }
  omegat := twopi * f * pred(t) / maxarraysize;
end;

{ main test routine starts here }

end.

