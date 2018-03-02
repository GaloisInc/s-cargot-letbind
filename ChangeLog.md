# Revision history for s-cargot-letbind

## 0.2.1.0  -- 2018-03-02

   * Updated to use s-cargot release 0.1.3.0 and the
     `unconstrainedOutput` added in that version for increased
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

