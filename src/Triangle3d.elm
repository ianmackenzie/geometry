--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Triangle3d exposing
    ( Triangle3d
    , fromVertices, on
    , vertices, edges, centroid, area, normalDirection, boundingBox, circumcircle
    , scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, mapVertices
    , relativeTo, placeIn, projectInto
    )

{-| A `Triangle3d` represents a triangle in 3D space, and is defined by its
three vertices. This module contains triangle-related functionality such as:

  - Finding the area and centroid of triangles
  - Scaling, rotating, translating, mirroring and projecting triangles
  - Converting triangles between different coordinate systems

@docs Triangle3d


# Constructors

@docs fromVertices, on


# Properties

@docs vertices, edges, centroid, area, normalDirection, boundingBox, circumcircle


# Transformations

Transforming a triangle is equivalent to transforming its vertices.

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, mapVertices


# Coordinate conversions

@docs relativeTo, placeIn, projectInto

-}

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Circle3d exposing (Circle3d)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import LineSegment3d exposing (LineSegment3d)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity, Squared)
import Quantity.Extra as Quantity
import SketchPlane3d exposing (SketchPlane3d)
import Triangle2d exposing (Triangle2d)
import Vector3d exposing (Vector3d)


{-| -}
type alias Triangle3d units coordinates =
    Types.Triangle3d units coordinates


{-| Construct a triangle from its three vertices:

    exampleTriangle =
        Triangle3d.fromVertices
            ( Point3d.meters 1 0 0
            , Point3d.meters 2 0 0
            , Point3d.meters 2 1 3
            )

-}
fromVertices : Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates -> Triangle3d units coordinates
fromVertices p1 p2 p3 =
    Types.Triangle3d ( p1, p2, p3 )


{-| Construct a 3D triangle lying _on_ a sketch plane by providing a 2D triangle
specified in XY coordinates _within_ the sketch plane.

    Triangle3d.on SketchPlane3d.xz <|
        Triangle2d.fromVertices
            ( Point2d.meters 1 1
            , Point2d.meters 2 1
            , Point2d.meters 1 3
            )
    --> Triangle3d.fromVertices
    -->     ( Point3d.meters 1 0 1
    -->     , Point3d.meters 2 0 1
    -->     , Point3d.meters 1 0 3
    -->     )

-}
on : SketchPlane3d units coordinates3d { defines : coordinates2d } -> Triangle2d units coordinates2d -> Triangle3d units coordinates3d
on sketchPlane triangle2d =
    let
        ( p1, p2, p3 ) =
            Triangle2d.vertices triangle2d
    in
    fromVertices
        (Point3d.on sketchPlane p1)
        (Point3d.on sketchPlane p2)
        (Point3d.on sketchPlane p3)


{-| Get the vertices of a triangle.


    ( p1, p2, p3 ) =
        Triangle3d.vertices exampleTriangle

    --> p1 = Point3d.meters 1 0 0
    --> p2 = Point3d.meters 2 0 0
    --> p3 = Point3d.meters 2 1 3

-}
vertices : Triangle3d units coordinates -> ( Point3d units coordinates, Point3d units coordinates, Point3d units coordinates )
vertices (Types.Triangle3d triangleVertices) =
    triangleVertices


{-| Get the edges of a triangle: from the first vertex to the second, from the
second to the third, and from the third back to the first.


    ( e1, e2, e3 ) =
        Triangle3d.edges exampleTriangle

    --> e1 =
    -->     LineSegment3d.fromEndpoints
    -->         ( Point3d.meters 1 0 0
    -->         , Point3d.meters 2 0 0
    -->         )
    -->
    --> e2 =
    -->     LineSegment3d.fromEndpoints
    -->         ( Point3d.meters 2 0 0
    -->         , Point3d.meters 2 1 3
    -->         )
    -->
    --> e3 =
    -->     LineSegment3d.fromEndpoints
    -->         ( Point3d.meters 2 1 3
    -->         , Point3d.meters 1 0 0
    -->         )

-}
edges : Triangle3d units coordinates -> ( LineSegment3d units coordinates, LineSegment3d units coordinates, LineSegment3d units coordinates )
edges triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle
    in
    ( LineSegment3d.from p1 p2
    , LineSegment3d.from p2 p3
    , LineSegment3d.from p3 p1
    )


{-| Get the centroid (center of mass) of a triangle.

    Triangle3d.centroid exampleTriangle
    --> Point3d.meters 1.6667 0.6667 1

-}
centroid : Triangle3d units coordinates -> Point3d units coordinates
centroid triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle
    in
    Point3d.centroid3 p1 p2 p3


{-| Get the area of a triangle. This value is always positive.

    Triangle3d.area exampleTriangle
    --> 1.5811

-}
area : Triangle3d units coordinates -> Quantity Float (Squared units)
area triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        firstVector =
            Vector3d.from p1 p2

        secondVector =
            Vector3d.from p1 p3
    in
    Quantity.multiplyBy 0.5
        (Vector3d.length (firstVector |> Vector3d.cross secondVector))


{-| Attempt to find the normal direction to a triangle. The resulting direction
will be oriented such that the triangle vertices are in counterclockwise order
around it according to the right-hand rule. If the triangle is degenerate (its
three vertices are collinear), returns `Nothing`.

    Triangle3d.normalDirection exampleTriangle
    --> Just
    -->     (Direction3d.fromAzimuthAndElevation
    -->         (Angle.degrees -90)
    -->         (Angle.degrees 18.43)
    -->     )

-}
normalDirection : Triangle3d units coordinates -> Maybe (Direction3d coordinates)
normalDirection triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        v1 =
            Vector3d.from p1 p2

        v2 =
            Vector3d.from p2 p3
    in
    Vector3d.direction (v1 |> Vector3d.cross v2)


{-| Scale a triangle about a given point by a given scale.

    Triangle3d.scaleAbout Point3d.origin 2 exampleTriangle
    --> Triangle3d.fromVertices
    -->     ( Point3d.meters 2 0 0
    -->     , Point3d.meters 4 0 0
    -->     , Point3d.meters 4 2 6
    -->     )

-}
scaleAbout : Point3d units coordinates -> Float -> Triangle3d units coordinates -> Triangle3d units coordinates
scaleAbout centerPoint scale triangle =
    mapVertices (Point3d.scaleAbout centerPoint scale) triangle


{-| Rotate a triangle around a given axis by a given angle (in radians).

    exampleTriangle
        |> Triangle3d.rotateAround Axis3d.z (Angle.degrees 90)
    --> Triangle3d.fromVertices
    -->     ( Point3d.meters 0 1 0
    -->     , Point3d.meters 0 2 0
    -->     , Point3d.meters -1 2 3
    -->     )

-}
rotateAround : Axis3d units coordinates -> Angle -> Triangle3d units coordinates -> Triangle3d units coordinates
rotateAround axis angle triangle =
    mapVertices (Point3d.rotateAround axis angle) triangle


{-| Translate a triangle by a given displacement.

    displacement =
        Vector3d.meters 2 -1 3

    Triangle3d.translateBy displacement exampleTriangle
    --> Triangle3d.fromVertices
    -->     ( Point3d.meters 3 -1 3
    -->     , Point3d.meters 4 -1 3
    -->     , Point3d.meters 4 0 6
    -->     )

-}
translateBy : Vector3d units coordinates -> Triangle3d units coordinates -> Triangle3d units coordinates
translateBy vector triangle =
    mapVertices (Point3d.translateBy vector) triangle


{-| Translate a triangle in a given direction by a given distance;

    Triangle3d.translateIn direction distance

is equivalent to

    Triangle3d.translateBy
        (Vector3d.withLength distance direction)

-}
translateIn : Direction3d coordinates -> Quantity Float units -> Triangle3d units coordinates -> Triangle3d units coordinates
translateIn direction distance triangle =
    translateBy (Vector3d.withLength distance direction) triangle


{-| Mirror a triangle across a given plane.

    Triangle3d.mirrorAcross Plane3d.yz exampleTriangle
    --> Triangle3d.fromVertices
    -->     ( Point3d.meters -1 0 0
    -->     ( Point3d.meters -2 0 0
    -->     ( Point3d.meters -2 1 3
    -->     )

-}
mirrorAcross : Plane3d units coordinates -> Triangle3d units coordinates -> Triangle3d units coordinates
mirrorAcross plane triangle =
    mapVertices (Point3d.mirrorAcross plane) triangle


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a triangle onto a plane.

    Triangle3d.projectOnto Plane3d.xy exampleTriangle
    --> Triangle3d.fromVertices
    -->     ( Point3d.meters 1 0 0
    -->     , Point3d.meters 2 0 0
    -->     , Point3d.meters 2 1 0
    -->     )

    Triangle3d.projectOnto Plane3d.xz exampleTriangle
    --> Triangle3d.fromVertices
    -->     ( Point3d.meters 1 0 0
    -->     , Point3d.meters 2 0 0
    -->     , Point3d.meters 2 0 3
    -->     )

-}
projectOnto : Plane3d units coordinates -> Triangle3d units coordinates -> Triangle3d units coordinates
projectOnto plane triangle =
    mapVertices (Point3d.projectOnto plane) triangle


{-| Transform each vertex of a triangle by a given function and create a new
triangle from the resulting points. Most other transformation functions can be
defined in terms of `mapVertices`; for example,

    Triangle3d.projectOnto plane

is equivalent to

    Triangle3d.mapVertices (Point3d.projectOnto plane)

-}
mapVertices : (Point3d units1 coordinates1 -> Point3d units2 coordinates2) -> Triangle3d units1 coordinates1 -> Triangle3d units2 coordinates2
mapVertices function triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle
    in
    fromVertices (function p1) (function p2) (function p3)


{-| Take a triangle defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.

    localFrame =
        Frame3d.atPoint
            (Point3d.meters 2 1 3)

    Triangle3d.relativeTo localFrame exampleTriangle
    --> Triangle3d.fromVertices
    -->     ( Point3d.meters -1 -1 -3
    -->     , Point3d.meters 0 -1 -3
    -->     , Point3d.meters 0 0 0
    -->     )

-}
relativeTo : Frame3d units globalCoordinates { defines : localCoordinates } -> Triangle3d units globalCoordinates -> Triangle3d units localCoordinates
relativeTo frame triangle =
    mapVertices (Point3d.relativeTo frame) triangle


{-| Take a triangle considered to be defined in local coordinates relative to a
given reference frame, and return that triangle expressed in global coordinates.

    localFrame =
        Frame3d.atPoint
            (Point3d.meters 2 1 3)

    Triangle3d.placeIn localFrame exampleTriangle
    --> Triangle3d.fromVertices
    -->     ( Point3d.meters 3 1 3
    -->     , Point3d.meters 4 1 3
    -->     , Point3d.meters 4 2 6
    -->     )

-}
placeIn : Frame3d units globalCoordinates { defines : localCoordinates } -> Triangle3d units localCoordinates -> Triangle3d units globalCoordinates
placeIn frame triangle =
    mapVertices (Point3d.placeIn frame) triangle


{-| Project a triangle into a given sketch plane. Conceptually, this finds the
[orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of the triangle onto the plane and then expresses the projected triangle in 2D
sketch coordinates.

    Triangle3d.projectInto SketchPlane3d.xy exampleTriangle
    --> Triangle2d.fromVertices
    -->     ( Point2d.meters 1 0
    -->     , Point2d.meters 2 0
    -->     , Point2d.meters 2 1
    -->     )

    Triangle3d.projectInto SketchPlane3d.zx exampleTriangle
    --> Triangle2d.fromVertices
    -->     ( Point2d.meters 0 1
    -->     , Point2d.meters 0 2
    -->     , Point2d.meters 3 2
    -->     )

-}
projectInto : SketchPlane3d units coordinates3d { defines : coordinates2d } -> Triangle3d units coordinates3d -> Triangle2d units coordinates2d
projectInto sketchPlane triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        project =
            Point3d.projectInto sketchPlane
    in
    Triangle2d.fromVertices (project p1) (project p2) (project p3)


{-| Get the minimal bounding box containing a given triangle.

    Triangle3d.boundingBox exampleTriangle
    --> BoundingBox3d.fromExtrema
    -->     { minX = 1
    -->     , maxX = 2
    -->     , minY = 0
    -->     , maxY = 1
    -->     , minZ = 0
    -->     , maxZ = 3
    -->     }

-}
boundingBox : Triangle3d units coordinates -> BoundingBox3d units coordinates
boundingBox triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        x1 =
            Point3d.xCoordinate p1

        y1 =
            Point3d.yCoordinate p1

        z1 =
            Point3d.zCoordinate p1

        x2 =
            Point3d.xCoordinate p2

        y2 =
            Point3d.yCoordinate p2

        z2 =
            Point3d.zCoordinate p2

        x3 =
            Point3d.xCoordinate p3

        y3 =
            Point3d.yCoordinate p3

        z3 =
            Point3d.zCoordinate p3
    in
    BoundingBox3d.fromExtrema
        { minX = Quantity.min x1 (Quantity.min x2 x3)
        , maxX = Quantity.max x1 (Quantity.max x2 x3)
        , minY = Quantity.min y1 (Quantity.min y2 y3)
        , maxY = Quantity.max y1 (Quantity.max y2 y3)
        , minZ = Quantity.min z1 (Quantity.min z2 z3)
        , maxZ = Quantity.max z1 (Quantity.max z2 z3)
        }


{-| Attempt to find the circumcircle of a triangle, a circle that passes through
each of the triangle's vertices;

    Triangle3d.circumcircle triangle

is equivalent to

    ( p1, p2, p3 ) =
        Triangle3d.vertices triangle

    Circle3d.throughPoints p1 p2 p3

If the triangle is degenerate (its three vertices are collinear), returns
`Nothing`.

-}
circumcircle : Triangle3d units coordinates -> Maybe (Circle3d units coordinates)
circumcircle triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle
    in
    Circle3d.throughPoints p1 p2 p3
