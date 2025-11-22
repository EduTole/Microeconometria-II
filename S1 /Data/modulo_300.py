import pandas as pd
import numpy as np

# ==============================================
# Codigos de ENAHO: modulo 300
class process300(object):
    def __init__(self, frame:pd.DataFrame):
        self.frame = frame.copy()
        self.frame.columns = self.frame.columns.str.lower()
        
    def func_hogar(self):
        '''el siguiente metodo permitira poder concatenar'''
        self.frame['rcod_hogar']=(self.frame['conglome'].astype(str)+
                                 self.frame['vivienda'].astype(str)+
                                 self.frame['hogar'].astype(str))
        return self
    
    def func_person(self):
        self.frame['rcod_person']=(self.frame['conglome'].astype(str)+
                                 self.frame['vivienda'].astype(str)+
                                 self.frame['hogar'].astype(str) + 
                                 self.frame['codperso'].astype(str))
        return self 
        
    def func_reduca(self):
        pa = pd.to_numeric(self.frame['p301a'])
        pb = pd.to_numeric(self.frame['p301b'])
        pc = pd.to_numeric(self.frame['p301c'])
            
        self.frame['rneduca'] = pb 
        self.frame.loc[(pa>=1) & (pa<=4),\
                       'rneduca'] = (self.frame['rneduca'] + 0)
        self.frame.loc[(pa>=5) & (pa<=6), \
                       'rneduca'] = (self.frame['rneduca'] + 6)
        self.frame.loc[(pa>=7) & (pa<=10),\
                       'rneduca']= (self.frame['rneduca'] + 11)
        self.frame.loc[(pa==11),\
                       'rneduca' ]= (self.frame['rneduca'] + 16)

        return self.frame 

