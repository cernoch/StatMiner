package cernoch.sm.space

/**
 * Thrown when evaluating a state,
 * which should not be explored at all
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class TooOld(msg: String)
  extends RuntimeException(msg) {}

/**
 * Thrown when evaluating a state,
 * which is too immature to be evaluated
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class TooNew(msg: String)
  extends RuntimeException(msg) {}
