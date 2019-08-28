--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Sphere3d exposing
    ( Sphere3d
    , withRadius, throughPoints
    , centerPoint, radius, diameter, volume, surfaceArea, circumference, boundingBox
    , contains
    , scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, projectInto
    , relativeTo, placeIn
    )

{-| A `Sphere3d` is defined by its center point and radius. This module contains
functionality for:

  - Constructing spheres through points
  - Scaling, rotating and translating spheres
  - Extracting sphere properties like center point and volume

@docs Sphere3d


# Constructors

@docs withRadius, throughPoints


# Properties

@docs centerPoint, radius, diameter, volume, surfaceArea, circumference, boundingBox


# Queries

@docs contains


# Transformations

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, projectInto


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Circle2d exposing (Circle2d)
import Circle3d exposing (Circle3d)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types exposing (Sphere3d)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity exposing (Cubed, Quantity, Squared)
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)



{--Imports for verifying the examples:

    import Geometry.Examples.Sphere3d exposing (..)
    import Geometry.Examples.Expect as Expect
    import Axis3d
    import BoundingBox3d
    import Circle2d
    import Circle3d
    import Direction3d
    import Float.Extra as Float
    import Frame3d exposing (Frame3d)
    import Plane3d
    import Point2d
    import Point3d exposing (Point3d)
    import SketchPlane3d
    import Sphere3d
    import Vector3d exposing (Vector3d)
-}


{-| -}
type alias Sphere3d units coordinates =
    Types.Sphere3d units coordinates


{-| Construct a sphere from its radius and center point:

    exampleSphere =
        Sphere3d.withRadius 3
            (Point3d.meters 1 2 1)

If you pass a negative radius, the absolute value will be used.

-}
withRadius : Quantity Float units -> Point3d units coordinates -> Sphere3d units coordinates
withRadius givenRadius givenCenterPoint =
    Types.Sphere3d
        { radius = Quantity.abs givenRadius
        , centerPoint = givenCenterPoint
        }


{-| Attempt to construct a sphere that passes through the four given points.
Returns `Nothing` if four given points are coplanar.

    Sphere3d.throughPoints
        (Point3d.meters 1 0 0)
        (Point3d.meters -1 0 0)
        (Point3d.meters 0 1 0)
        (Point3d.meters 0 0 0.5)
    --> Just
    -->     (Sphere3d.withRadius 1.25
    -->         (Point3d.meters 0 0 -0.75)
    -->     )

    Sphere3d.throughPoints
        (Point3d.meters 1 0 0)
        (Point3d.meters -1 0 0)
        (Point3d.meters 0 1 0)
        (Point3d.meters 0 -1 0)
    --> Nothing

-}
throughPoints : Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates -> Maybe (Sphere3d units coordinates)
throughPoints p1 p2 p3 p4 =
    Circle3d.throughPoints p1 p2 p3
        |> Maybe.andThen
            {-
               First three points define a circle.
               All points M on the normal to this circle through the circle's center are equidistant to p1, p2 and p3.
               Therefore: find this point M such that it is equidistant to p1, p2, p3 and p4,
               this will be the center of the sphere.
            -}
            (\circle ->
                let
                    normalAxis =
                        Circle3d.axis circle

                    r =
                        Circle3d.radius circle

                    x =
                        Point3d.distanceFromAxis normalAxis p4

                    y =
                        Point3d.signedDistanceAlong normalAxis p4
                in
                if y /= Quantity.zero then
                    let
                        d =
                            (Quantity.squared r
                                |> Quantity.minus (Quantity.squared x)
                                |> Quantity.minus (Quantity.squared y)
                            )
                                |> Quantity.over
                                    (Quantity.multiplyBy -2 y)

                        computedRadius =
                            Quantity.sqrt
                                (Quantity.squared r
                                    |> Quantity.plus
                                        (Quantity.squared d)
                                )
                    in
                    Just <|
                        withRadius computedRadius (Point3d.along normalAxis d)

                else
                    Nothing
            )


{-| Get the center point of a sphere.

    Sphere3d.centerPoint exampleSphere
    --> Point3d.meters 1 2 1

-}
centerPoint : Sphere3d units coordinates -> Point3d units coordinates
centerPoint (Types.Sphere3d properties) =
    properties.centerPoint


{-| Get the radius of a sphere.

    Sphere3d.radius exampleSphere
    --> 3

-}
radius : Sphere3d units coordinates -> Quantity Float units
radius (Types.Sphere3d properties) =
    properties.radius


{-| Get the diameter of a sphere.

    Sphere3d.diameter exampleSphere
    --> 6

-}
diameter : Sphere3d units coordinates -> Quantity Float units
diameter sphere =
    Quantity.multiplyBy 2 (radius sphere)


{-| Get the circumference of a sphere (the circumference of a [great circle](https://en.wikipedia.org/wiki/Great_circle)
of the sphere).

    Sphere3d.circumference exampleSphere
    --> 18.8496

-}
circumference : Sphere3d units coordinates -> Quantity Float units
circumference sphere =
    Quantity.multiplyBy (2 * pi) (radius sphere)


{-| Get the surface area of a sphere.

    Sphere3d.surfaceArea exampleSphere
    --> 113.0973

-}
surfaceArea : Sphere3d units coordinates -> Quantity Float (Squared units)
surfaceArea sphere =
    Quantity.multiplyBy (4 * pi) (Quantity.squared (radius sphere))


{-| Get the volume of a sphere.

    Sphere3d.volume exampleSphere
    --> 113.0973

-}
volume : Sphere3d units coordinates -> Quantity Float (Cubed units)
volume sphere =
    Quantity.multiplyBy (4 / 3 * pi) (Quantity.cubed (radius sphere))


{-| Scale a sphere around a given point by a given scale.

    Sphere3d.scaleAbout Point3d.origin 3 exampleSphere
    --> Sphere3d.withRadius 9
    -->     (Point3d.meters 3 6 3)

-}
scaleAbout : Point3d units coordinates -> Float -> Sphere3d units coordinates -> Sphere3d units coordinates
scaleAbout point scale sphere =
    withRadius (Quantity.multiplyBy (abs scale) (radius sphere))
        (Point3d.scaleAbout point scale (centerPoint sphere))


{-| Rotate a sphere around a given axis by a given angle (in radians).

    exampleSphere
        |> Sphere3d.rotateAround Axis3d.y (Angle.degrees 90)
    --> Sphere3d.withRadius 3
    -->     (Point3d.meters 1 2 -1)

-}
rotateAround : Axis3d units coordinates -> Angle -> Sphere3d units coordinates -> Sphere3d units coordinates
rotateAround axis angle sphere =
    withRadius (radius sphere)
        (Point3d.rotateAround axis angle (centerPoint sphere))


{-| Translate a sphere by a given displacement.

    exampleSphere
        |> Sphere3d.translateBy
            (Vector3d.meters 2 1 3)
    --> Sphere3d.withRadius 3
    -->     (Point3d.meters 3 3 4)

-}
translateBy : Vector3d units coordinates -> Sphere3d units coordinates -> Sphere3d units coordinates
translateBy displacement sphere =
    withRadius (radius sphere)
        (Point3d.translateBy displacement (centerPoint sphere))


{-| Translate a sphere in a given direction by a given distance;

    Sphere3d.translateIn direction distance

is equivalent to

    Sphere3d.translateBy
        (Vector3d.withLength distance direction)

-}
translateIn : Direction3d coordinates -> Quantity Float units -> Sphere3d units coordinates -> Sphere3d units coordinates
translateIn direction distance sphere =
    translateBy (Vector3d.withLength distance direction) sphere


{-| Mirror a sphere across a given plane.

    Sphere3d.mirrorAcross Plane3d.xy exampleSphere
    --> Sphere3d.withRadius 3
    -->     (Point3d.meters 1 2 -1)

-}
mirrorAcross : Plane3d units coordinates -> Sphere3d units coordinates -> Sphere3d units coordinates
mirrorAcross plane sphere =
    withRadius (radius sphere)
        (Point3d.mirrorAcross plane (centerPoint sphere))


{-| Take a sphere defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    exampleSphere
        |> Sphere3d.relativeTo
            (Frame3d.atPoint
                (Point3d.meters 1 2 3)
            )
    --> Sphere3d.withRadius 3
    -->     (Point3d.meters 0 0 -2)

-}
relativeTo : Frame3d units globalCoordinates { defines : localCoordinates } -> Sphere3d units globalCoordinates -> Sphere3d units localCoordinates
relativeTo frame sphere =
    withRadius (radius sphere)
        (Point3d.relativeTo frame (centerPoint sphere))


{-| Take a sphere considered to be defined in local coordinates relative to a
given reference frame, and return that sphere expressed in global coordinates.

    exampleSphere
        |> Sphere3d.placeIn
            (Frame3d.atPoint
                (Point3d.meters 1 2 3)
            )
    --> Sphere3d.withRadius 3
    -->     (Point3d.meters 2 4 4)

-}
placeIn : Frame3d units globalCoordinates { defines : localCoordinates } -> Sphere3d units localCoordinates -> Sphere3d units globalCoordinates
placeIn frame sphere =
    withRadius (radius sphere)
        (Point3d.placeIn frame (centerPoint sphere))


{-| Get the minimal bounding box containing a given sphere.

    Sphere3d.boundingBox exampleSphere
    --> BoundingBox3d.fromExtrema
    -->     { minX = -2
    -->     , maxX = 4
    -->     , minY = -1
    -->     , maxY = 5
    -->     , minZ = -2
    -->     , maxZ = 4
    -->     }

-}
boundingBox : Sphere3d units coordinates -> BoundingBox3d units coordinates
boundingBox sphere =
    let
        r =
            radius sphere

        p0 =
            centerPoint sphere

        cx =
            Point3d.xCoordinate p0

        cy =
            Point3d.yCoordinate p0

        cz =
            Point3d.zCoordinate p0
    in
    BoundingBox3d.fromExtrema
        { minX = cx |> Quantity.minus r
        , maxX = cx |> Quantity.plus r
        , minY = cy |> Quantity.minus r
        , maxY = cy |> Quantity.plus r
        , minZ = cz |> Quantity.minus r
        , maxZ = cz |> Quantity.plus r
        }


{-| Check if a sphere contains a given point.

    Sphere3d.contains
        (Point3d.meters 4 2 1)
        exampleSphere
    --> True

    Sphere3d.contains
        (Point3d.meters 4.00001 2 1)
        exampleSphere
    --> False

-}
contains : Point3d units coordinates -> Sphere3d units coordinates -> Bool
contains point sphere =
    point |> Point3d.distanceFrom (centerPoint sphere) |> Quantity.lessThanOrEqualTo (radius sphere)


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a sphere onto a plane.

    Sphere3d.projectOnto Plane3d.xy exampleSphere
    --> Circle3d.withRadius 3
    -->     Direction3d.z
    -->     (Point3d.meters 1 2 0)

-}
projectOnto : Plane3d units coordinates -> Sphere3d units coordinates -> Circle3d units coordinates
projectOnto plane sphere =
    Circle3d.withRadius (radius sphere)
        (Plane3d.normalDirection plane)
        (Point3d.projectOnto plane (centerPoint sphere))


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a sphere into a sketch plane.

    Sphere3d.projectInto SketchPlane3d.xy exampleSphere
    --> Circle2d.withRadius 3
    -->     (Point2d.meters 1 2)

-}
projectInto : SketchPlane3d units coordinates3d { defines : coordinates2d } -> Sphere3d units coordinates3d -> Circle2d units coordinates2d
projectInto sketchPlane sphere =
    Circle2d.withRadius (radius sphere)
        (Point3d.projectInto sketchPlane (centerPoint sphere))
