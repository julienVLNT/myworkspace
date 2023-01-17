# -*- coding: utf-8 -*-


import numpy as np
import os


__author__  = "Julien VALENTIN"
__date__    = "January, 2023"
__email__   = "julien.valentin@umontpellier.fr"
__version__ = "alpha"


class Point():

    def __init__(self, coord: np.ndarray, id: int, is_on_boundary: bool, region: int) -> None:
        assert coord.ndim == 1 and coord.shape[0] == 3, "L'objet doit être un point de R^3."
        assert id > 0, "L'identifiant d'un point doit être un entier strictement positif."
        assert region > 0, "La région, ou le matériau, associé à un point doit être un entier strictement positif."
        self.coord          = coord
        self.id             = id
        self.is_on_boundary = is_on_boundary
        self.region         = region


class Edge():

    def __init__(self, vertices: list, id: int) -> None:
        assert len(vertices) == 2, "Une arête est représentée par un couple de points."
        assert id > 0, "L'identifiant d'une arête doit être un entier strictement positif."
        self.vertices = vertices
        self.id       = id


class Face():
    
    def __init__(self, edges: list(Edge), id: int) -> None:
        assert len(edges) == 3, "Une face triangulaire est représentée par ses trois arêtes."
        assert id > 0, "L'identifiant d'une face doit être un entier strictement positif."
        self.edges = edges
        self.id    = id


class Element():

    def __init__(self, faces: list, id: int) -> None:
        assert len(faces) == 3, "Un élément tétrahédrique est représenté par ses quatre faces."
        assert id > 0, "L'identifiant d'un élément doit être un entier strictement positif."
        self.faces = faces
        self.id    = id


class Topology():

    def __init__(self, elements: list) -> None:
        self.elements = elements
