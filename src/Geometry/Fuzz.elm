--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--                                                                            --
-- Copyright 2016 by Ian Mackenzie                                            --
-- ian.e.mackenzie@gmail.com                                                  --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Geometry.Fuzz
    exposing
        ( arc2d
        , arc3d
        , axis2d
        , axis3d
        , boundingBox2d
        , boundingBox3d
        , circle2d
        , circle3d
        , cubicSpline2d
        , cubicSpline3d
        , direction2d
        , direction3d
        , ellipse2d
        , ellipticalArc2d
        , frame2d
        , frame3d
        , lineSegment2d
        , lineSegment3d
        , plane3d
        , point2d
        , point3d
        , polygon2d
        , polyline2d
        , polyline3d
        , quadraticSpline2d
        , quadraticSpline3d
        , scalar
        , sketchPlane3d
        , sphere3d
        , triangle2d
        , triangle3d
        , vector2d
        , vector3d
        )

import Arc2d exposing (Arc2d)
import Arc3d exposing (Arc3d)
import Axis2d exposing (Axis2d)
import Axis3d exposing (Axis3d)
import BoundingBox2d exposing (BoundingBox2d)
import BoundingBox3d exposing (BoundingBox3d)
import Circle2d exposing (Circle2d)
import Circle3d exposing (Circle3d)
import CubicSpline2d exposing (CubicSpline2d)
import CubicSpline3d exposing (CubicSpline3d)
import Direction2d exposing (Direction2d)
import Direction3d exposing (Direction3d)
import Ellipse2d exposing (Ellipse2d)
import EllipticalArc2d exposing (EllipticalArc2d)
import Frame2d exposing (Frame2d)
import Frame3d exposing (Frame3d)
import Fuzz exposing (Fuzzer)
import LineSegment2d exposing (LineSegment2d)
import LineSegment3d exposing (LineSegment3d)
import Plane3d exposing (Plane3d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import Polyline3d exposing (Polyline3d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import QuadraticSpline3d exposing (QuadraticSpline3d)
import SketchPlane3d exposing (SketchPlane3d)
import Sphere3d exposing (Sphere3d)
import Triangle2d exposing (Triangle2d)
import Triangle3d exposing (Triangle3d)
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)


scalar : Fuzzer Float
scalar =
    Fuzz.floatRange -10 10


positiveScalar : Fuzzer Float
positiveScalar =
    Fuzz.map abs scalar


vector2d : Fuzzer Vector2d
vector2d =
    Fuzz.map Vector2d.fromComponents (Fuzz.tuple ( scalar, scalar ))


vector3d : Fuzzer Vector3d
vector3d =
    Fuzz.map Vector3d.fromComponents (Fuzz.tuple3 ( scalar, scalar, scalar ))


direction2d : Fuzzer Direction2d
direction2d =
    Fuzz.map Direction2d.fromAngle (Fuzz.floatRange -pi pi)


direction3d : Fuzzer Direction3d
direction3d =
    let
        phi =
            Fuzz.map acos (Fuzz.floatRange -1 1)

        theta =
            Fuzz.floatRange -pi pi

        direction phi theta =
            let
                r =
                    sin phi

                x =
                    r * cos theta

                y =
                    r * sin theta

                z =
                    cos phi
            in
            Direction3d.unsafe ( x, y, z )
    in
    Fuzz.map2 direction phi theta


point2d : Fuzzer Point2d
point2d =
    Fuzz.map Point2d.fromCoordinates (Fuzz.tuple ( scalar, scalar ))


point3d : Fuzzer Point3d
point3d =
    Fuzz.map Point3d.fromCoordinates (Fuzz.tuple3 ( scalar, scalar, scalar ))


axis2d : Fuzzer Axis2d
axis2d =
    let
        axis originPoint direction =
            Axis2d.with { originPoint = originPoint, direction = direction }
    in
    Fuzz.map2 axis point2d direction2d


axis3d : Fuzzer Axis3d
axis3d =
    let
        axis originPoint direction =
            Axis3d.with { originPoint = originPoint, direction = direction }
    in
    Fuzz.map2 axis point3d direction3d


plane3d : Fuzzer Plane3d
plane3d =
    let
        plane originPoint normalDirection =
            Plane3d.with
                { originPoint = originPoint
                , normalDirection = normalDirection
                }
    in
    Fuzz.map2 plane point3d direction3d


frame2d : Fuzzer Frame2d
frame2d =
    let
        frame originPoint xDirection rightHanded =
            let
                rightHandedFrame =
                    Frame2d.with
                        { originPoint = originPoint
                        , xDirection = xDirection
                        }
            in
            if rightHanded then
                rightHandedFrame
            else
                Frame2d.flipY rightHandedFrame
    in
    Fuzz.map3 frame point2d direction2d Fuzz.bool


frame3d : Fuzzer Frame3d
frame3d =
    let
        frame originPoint xDirection flipY flipZ =
            let
                ( yDirection, zDirection ) =
                    Direction3d.perpendicularBasis xDirection
            in
            Frame3d.unsafe
                { originPoint = originPoint
                , xDirection = xDirection
                , yDirection =
                    if flipY then
                        Direction3d.flip yDirection
                    else
                        yDirection
                , zDirection =
                    if flipZ then
                        Direction3d.flip zDirection
                    else
                        zDirection
                }
    in
    Fuzz.map4 frame point3d direction3d Fuzz.bool Fuzz.bool


sketchPlane3d : Fuzzer SketchPlane3d
sketchPlane3d =
    let
        sketchPlane originPoint xDirection =
            SketchPlane3d.unsafe
                { originPoint = originPoint
                , xDirection = xDirection
                , yDirection = Direction3d.perpendicularTo xDirection
                }
    in
    Fuzz.map2 sketchPlane point3d direction3d


lineSegment2d : Fuzzer LineSegment2d
lineSegment2d =
    Fuzz.map LineSegment2d.fromEndpoints (Fuzz.tuple ( point2d, point2d ))


lineSegment3d : Fuzzer LineSegment3d
lineSegment3d =
    Fuzz.map LineSegment3d.fromEndpoints (Fuzz.tuple ( point3d, point3d ))


triangle2d : Fuzzer Triangle2d
triangle2d =
    Fuzz.map Triangle2d.fromVertices (Fuzz.tuple3 ( point2d, point2d, point2d ))


triangle3d : Fuzzer Triangle3d
triangle3d =
    Fuzz.map Triangle3d.fromVertices (Fuzz.tuple3 ( point3d, point3d, point3d ))


boundingBox2d : Fuzzer BoundingBox2d
boundingBox2d =
    Fuzz.map BoundingBox2d.fromCorners (Fuzz.map2 (,) point2d point2d)


boundingBox3d : Fuzzer BoundingBox3d
boundingBox3d =
    Fuzz.map BoundingBox3d.fromCorners (Fuzz.map2 (,) point3d point3d)


polyline2d : Fuzzer Polyline2d
polyline2d =
    Fuzz.map Polyline2d.fromVertices (Fuzz.list point2d)


polyline3d : Fuzzer Polyline3d
polyline3d =
    Fuzz.map Polyline3d.fromVertices (Fuzz.list point3d)


polygon2d : Fuzzer Polygon2d
polygon2d =
    Fuzz.map2 Polygon2d.withHoles
        (Fuzz.list point2d)
        (Fuzz.list (Fuzz.list point2d))


circle2d : Fuzzer Circle2d
circle2d =
    let
        circle centerPoint radius =
            Circle2d.with { centerPoint = centerPoint, radius = radius }
    in
    Fuzz.map2 circle point2d positiveScalar


circle3d : Fuzzer Circle3d
circle3d =
    let
        circle centerPoint axialDirection radius =
            Circle3d.with
                { centerPoint = centerPoint
                , axialDirection = axialDirection
                , radius = radius
                }
    in
    Fuzz.map3 circle point3d direction3d positiveScalar


sphere3d : Fuzzer Sphere3d
sphere3d =
    let
        sphere centerPoint radius =
            Sphere3d.with { centerPoint = centerPoint, radius = radius }
    in
    Fuzz.map2 sphere point3d positiveScalar


arc2d : Fuzzer Arc2d
arc2d =
    Fuzz.map3 Arc2d.sweptAround
        point2d
        (Fuzz.floatRange (-3 * pi) (3 * pi))
        point2d


arc3d : Fuzzer Arc3d
arc3d =
    let
        arc axis startPoint sweptAngle =
            Arc3d.around axis
                { startPoint = startPoint
                , sweptAngle = sweptAngle
                }
    in
    Fuzz.map3 arc axis3d point3d (Fuzz.floatRange (-3 * pi) (3 * pi))


quadraticSpline2d : Fuzzer QuadraticSpline2d
quadraticSpline2d =
    Fuzz.tuple3 ( point2d, point2d, point2d )
        |> Fuzz.map QuadraticSpline2d.fromControlPoints


quadraticSpline3d : Fuzzer QuadraticSpline3d
quadraticSpline3d =
    Fuzz.tuple3 ( point3d, point3d, point3d )
        |> Fuzz.map QuadraticSpline3d.fromControlPoints


cubicSpline2d : Fuzzer CubicSpline2d
cubicSpline2d =
    Fuzz.tuple4 ( point2d, point2d, point2d, point2d )
        |> Fuzz.map CubicSpline2d.fromControlPoints


cubicSpline3d : Fuzzer CubicSpline3d
cubicSpline3d =
    Fuzz.tuple4 ( point3d, point3d, point3d, point3d )
        |> Fuzz.map CubicSpline3d.fromControlPoints


ellipse2d : Fuzzer Ellipse2d
ellipse2d =
    let
        ellipse centerPoint xDirection xRadius yRadius =
            Ellipse2d.with
                { centerPoint = centerPoint
                , xDirection = xDirection
                , xRadius = xRadius
                , yRadius = yRadius
                }
    in
    Fuzz.map4 ellipse point2d direction2d positiveScalar positiveScalar


ellipticalArc2d : Fuzzer EllipticalArc2d
ellipticalArc2d =
    let
        angle =
            Fuzz.floatRange -(2 * pi) (2 * pi)

        ellipticalArc ( centerPoint, xDirection ) ( xRadius, yRadius ) ( startAngle, sweptAngle ) =
            EllipticalArc2d.with
                { centerPoint = centerPoint
                , xDirection = xDirection
                , xRadius = xRadius
                , yRadius = yRadius
                , startAngle = startAngle
                , sweptAngle = sweptAngle
                }
    in
    Fuzz.map3 ellipticalArc
        (Fuzz.tuple ( point2d, direction2d ))
        (Fuzz.tuple ( positiveScalar, positiveScalar ))
        (Fuzz.tuple ( angle, angle ))
