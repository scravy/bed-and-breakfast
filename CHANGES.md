<dl>
  <dt>v0.1</dt>
  <dd>
    Initial version, features <code>det</code>,
    basic arithmetic operations, and instances for
    <code>Float</code>, <code>Double</code>, <code>Complex</code>, and
    <code>Rational</code>.
  </dd>

  <dt>v0.1.1</dt>
  <dd>
    Fixed wrong algorithm for computing the inverse of a <code>Matrix</code>.
  </dd>

  <dt>v0.1.2</dt>
  <dd>
    Added instances for <code>Num Matrix</code>,
    <code>Fractional Matrix</code>, and <code>Eq Matrix</code>.
  </dd>

  <dt>v0.1.3</dt>
  <dd>
    <code>inv</code> is now a total function and will no longer call
    <code>error</code> if a matrix is not invertible. Also <code>Matrix</code>
    derives <code>Data.Typeable</code>.
    now.
  </dd>

  <dt>v0.1.4</dt>
  <dd>
    Added <code>scale</code>, and methods for joining matrices vertically and
    horizontally. Corrected a bug in <code>isUnit</code> reported by Charles Durham.
    <code>isUnit</code> returned True for any matrix for which
    <code>all (== 1) . trace</code> would have, which is wrong).
  </dd>

  <dt>v0.2</dt>
  <dd>
    A little bit more documentation. Also moved some
    functions (<code>isXXX</code>) away from the type class <code>MatrixElement</code>.
    Properly flagged the package as experimental (was improperly marked as
    <code>stable</code>, copied form a template).
  </dd>

  <dt>v0.2.1</dt>
  <dd>
    Added <code>cofactors</code>, <code>adjugate</code>, <code>minor</code>, and
    <code>minorMatrix</code>.
  </dd>

  <dt>v0.2.2</dt>
  <dd>
    <code>rank</code> works now for any Matrix component type.
  </dd>

  <dt>v0.2.3</dt>
  <dd>
    Added <code>Read</code> instance for <code>Matrix</code>.
    Improved on documentation.
  </dd>

  <dt>v0.3</dt>
  <dd>
    Added a QuickCheck test suite, fixed a bug in <code>det</code>
    (det would crash for singular matrices, where it should
    return 0).
  </dd>

  <dt>v0.3.1</dt>
  <dd>
    Added TemplateHaskell syntactic sugar (see <code>Numeric.Matrix.Sugar</code>).
    Rewrote multiplication. <code>matrix</code> function build an array faster now.
  </dd>

  <dt>v0.3.2</dt>
  <dd>
    <code>Numeric.Matrix.Sugar</code> was not mentioned in the
    cabal file. Improved test suite. Improved documentation.
  </dd>

  <dt>v0.4</dt>
  <dd>
    Fixed a bug regarding <code>empty</code> and <code>fromList</code>.
    Use unsafe operations where it is safe for speed.
    Added RULES. Added an instance for binary.
  </dd>

  <dt>v0.4.1</dt>
  <dd>
    The unsafe operations used in v0.4 turned out
    to fatally fail on certain platforms. Revoked this change.
  </dd>

  <dt>v0.4.2</dt>
  <dd>
    Fixed a tiny bug regarding the <code>row</code> function
    for extracting the number of rows in a Matrix.
    Thanks to Tim Makarios for finding and fixing the bug.
  </dd>

  <dt>v0.4.3</dt>
  <dd>
    Fixed a bug in <code>transpose</code> that prevented it from
    working correctly with non-square matrices.
    Thanks to @owst@ from @hub.darcs.net@.
  </dd>

  <dt>v0.5</dt>
  <dd>
    <code>inv</code> works now for complex matrices too.
    Added conversion functions <code>toDoubleMatrix</code>,
    <code>toComplexMatrix</code>, <code>toRationalMatrix</code>.
    Changed signature of <code>minor</code> and <code>minorMatrix</code>
    to a more natural format.
  </dd>
</dl>

