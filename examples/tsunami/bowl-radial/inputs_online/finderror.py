import numpy as np
from read_amr import print_full
import pdb


class error_between_geoclaw(object):
    def __init__(self, test_class, original_class, type):
        self.test_class = test_class
        self.original_class = original_class
        self.type = type
#        self.mxv = test_class.mxv
#        self.myv = test_class.myv

#        self.land = self.get_land()
#        self.water = self.get_eta_with_land0()

    def get_land(self):
        #return self.test_class.land
        return self.original_class.land

    def eta_err(self):
#		pdb.set_trace()
        error_eta = self.test_class.get_eta_with_land0() -self.original_class.get_eta_with_land0()
#        if self.type == "relative":
#        elif self.type == "percent":
#            error_eta = (self.test_class.water -
#                         self.original_class.water)*100.0 / \
#                         self.original_class.water
            #mask_val = self.mxv**2 + self.myv**2 >= 8000
            #error_eta = np.ma.array(error_eta_temp, mask=mask_val)

#        else:
#            print "Type not consistent"
        #error_reshaped_masked_eta_water = np.ma.array(error_eta, mask=self.original_class.total_height == 0.0E0)
        #print "Maximum error = ", np.max(error_reshaped_masked_eta_water)
        #return error_reshaped_masked_eta_water
        return error_eta
