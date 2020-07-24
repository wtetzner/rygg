package org.bovinegenius.cmplang.modules;

public interface NamedNode<LOC extends Comparable<LOC>, NAME, SELF extends Node<LOC, NAME, SELF>> extends Node<LOC, NAME, SELF>, Named<LOC, NAME> {
}
