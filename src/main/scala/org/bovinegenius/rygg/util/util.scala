package org.bovinegenius.rygg.util


case class IntSequence(start: Int = 0) {
  private var current: Int = start

  def next: Int = {
    var value = current
    current += 1
    value
  }
}
