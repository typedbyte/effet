# Changelog for effet

## 0.1.0.0 (2020-07-17)

* Initial release

## 0.2.0.0 (2020-07-29)

* Lifted the restriction that one handler can exactly handle one effect. Handlers can now handle multiple effects.
* Effects whose definitions refer to other effects (see the RWS effect, for example) are now possible to implement, but only manually for now (i.e., no code generation).

## 0.3.0.0 (2020-10-13)

* Adapted the embed effect which now supports tagging/retagging/untagging.
* Introduced the managed effect.
* Relaxed bounds a bit for better compilation.

## 0.3.0.1 (2020-10-14)

* Fixed some compilation issues with GHC 8.6

## 0.3.0.2 (2020-10-21)

* Added Control.Effect.Identity for pure effect interpretations.