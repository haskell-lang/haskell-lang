-- | No longer active routes, these should all be redirects
module HL.Controller.Deprecated where

import HL.Controller

getOldTutorialsR :: C ()
getOldTutorialsR = redirect DocumentationR

getOldPackagesR :: C ()
getOldPackagesR = redirect LibrariesR

getOldPackageR :: PackageName -> C ()
getOldPackageR = redirect . LibraryR
