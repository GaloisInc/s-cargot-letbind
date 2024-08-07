# Revision history for s-cargot-letbind

## 0.2.5.0  -- 2024-08-07

  * Relax upper bounds to allowing building with GHC 9.8.

## 0.2.4.0  -- 2023-04-03

  * Remove upper bound on text (builtin to GHC)
  * Update for new s-cargot version with S-expression pretty printing whitespace
    and indentation fixes.

## 0.2.3.0  -- 2018-06-12

  * Relax upper bound to allow build with GHC 8.4

  * Haddock documentation fix for special characters.

## 0.2.2.0  -- 2018-03-02

  * Updated to use s-cargot release 0.1.4.0.

	More performance fixes in s-cargot for `basicPrint`, and using
	`unconstrainedPrint` without a width specification (because a
	width specification devolves into `basicPrint` anyhow).

## 0.2.1.0  -- 2018-03-02

  * Updated to use s-cargot release 0.1.3.0 and the
	`unconstrainedPrint` added in that version for increased
	performance.

## 0.2.0.0  -- 2018-02-16

  * Added verification ability to ensure let-bind variables are
	always unique with respect to each-other and with respect to all
	generated symbol strings within the s-expression.  This is
	performed internally within the 'discoverLetBindings' function,
	but requires the ability to get the string representation of an
	S-expression atom, so the 'extractStr' function is added to the
	'DiscoveryGuide' to support this.  If not provided, or if the
	provided form returns Nothing, no validation will be performed.

    * Suppress generation of let-binding phrase if there are no
	  discovered bindings to apply.

## 0.1.0.0  -- 2018-02-14

  * Initial version.
