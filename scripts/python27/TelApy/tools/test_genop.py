#! /usr/bin/python
# -*- coding: utf-8 -*-
"""
    Testing the 'genop' package with benchmark problems

    Author(s): Fabrice Zaoui

    Copyright EDF 2016

    Comments : genop... a genetic algorithm optimizer adapted from the original
               algorithm of Scilab module 'optim_ga'
"""

import genop
import numpy as np
import matplotlib.pyplot as plt
import unittest
import simul

class TestGenop(unittest.TestCase):
    """ Testing the Genop methods """
    def test_rosenbrock(self):
        f = simul.rosenbrock
        n = 2
        bd = np.zeros((n, 2))
        bd[0, 0] = -5.
        bd[0, 1] = 5.
        bd[1, 0] = -5.
        bd[1, 1] = 5.
        mypb = genop.Genop()
        mypb.verbose = True
        self.assertEqual(mypb.popsize, 100)
        self.assertEqual(mypb.pbcross, 0.7)
        self.assertEqual(mypb.pbmut, 0.1)
        error = mypb.initialize(f, n, bd)
        self.assertEqual(error, 0)
        fcost, xopt = mypb.optimize(20, nproc=2)
        self.assertTrue(fcost[-1] < 1.e-2)
        
    def test_maccormick(self):
        f = simul.maccormick
        n = 2
        bd = np.zeros((n, 2))
        bd[0, 0] = -1.5
        bd[0, 1] = 4.
        bd[1, 0] = -3.
        bd[1, 1] = 4.
        mypb = genop.Genop(90, 0.69, 0.05)
        mypb.verbose = False
        self.assertEqual(mypb.popsize, 90)
        self.assertEqual(mypb.pbcross, 0.69)
        self.assertEqual(mypb.pbmut, 0.05)
        error = mypb.initialize(f, n, bd)
        self.assertEqual(error, 0)
        fcost, xopt = mypb.optimize(20)
        self.assertTrue(fcost[-1] < -1.91)
        
    def test_easom(self):
        f = simul.easom
        n = 2
        bd = np.zeros((n, 2))
        bd[0, 0] = -100.
        bd[0, 1] = 100.
        bd[1, 0] = -100.
        bd[1, 1] = 100.
        mypb = genop.Genop()
        mypb.verbose = False
        self.assertEqual(mypb.popsize, 100)
        self.assertEqual(mypb.pbcross, 0.7)
        self.assertEqual(mypb.pbmut, 0.1)
        error = mypb.initialize(f, n, bd)
        self.assertEqual(error, 0)
        fcost, xopt = mypb.optimize(nbgen=100)
        self.assertTrue(np.abs(fcost[-2] - fcost[-1]) < 1.e-12)
        
    def test_eggholder(self):
        f = simul.eggholder
        n = 2
        bd = np.zeros((n, 2))
        bd[0, 0] = -512.
        bd[0, 1] = 512.
        bd[1, 0] = -512.
        bd[1, 1] = 512.
        mypb = genop.Genop(1000)
        mypb.verbose = False
        self.assertEqual(mypb.popsize, 1000)
        self.assertEqual(mypb.pbcross, 0.7)
        self.assertEqual(mypb.pbmut, 0.1)
        error = mypb.initialize(f, n, bd)
        self.assertEqual(error, 0)
        fcost, xopt = mypb.optimize(20)
        self.assertTrue(fcost[-1] < -900.)       
        
    def test_linear(self):
        f = simul.linear
        n = 3
        bd = np.zeros((n, 2))
        bd[0, 0] = -3.14
        bd[0, 1] = 3.14
        bd[1, 0] = 0.
        bd[1, 1] = 5.
        bd[2, 0] = -10.
        bd[2, 1] = 20.
        mypb = genop.Genop()
        mypb.verbose = False
        self.assertEqual(mypb.popsize, 100)
        self.assertEqual(mypb.pbcross, 0.7)
        self.assertEqual(mypb.pbmut, 0.1)
        error = mypb.initialize(f, n, bd)
        self.assertEqual(error, 0)
        fcost, xopt = mypb.optimize(200)
        self.assertTrue(fcost[-1] < -23.1)       
    
    def test_abslinear(self):
        f = simul.abslinear
        n = 1
        bd = np.zeros((n, 2))
        bd[0, 0] = -100.
        bd[0, 1] = 100.
        mypb = genop.Genop()
        mypb.verbose = False
        self.assertEqual(mypb.popsize, 100)
        self.assertEqual(mypb.pbcross, 0.7)
        self.assertEqual(mypb.pbmut, 0.1)
        error = mypb.initialize(f, n, bd)
        self.assertEqual(error, 0)
        fcost, xopt = mypb.optimize(2000)
        self.assertTrue(np.abs(xopt[-1] - 2.) < 1.e-3)           

if __name__ == '__main__':
    unittest.main()

