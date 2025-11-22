import pandas as pd
import numpy as np

# Codigos de ENAHO: modulo 34
class process34(object):
    def __init__(self, frame:pd.DataFrame):
        self.frame= frame.copy()
        self.frame.columns = self.frame.columns.str.lower()
    def func_hogar(self):
        '''el siguiente metodo permitira poder concatenar'''
        self.frame['rcod_hogar']=(self.frame['conglome'].astype(str)+
                                 self.frame['vivienda'].astype(str)+
                                 self.frame['hogar'].astype(str))
        return self
    
    def func_gasto(self):
        '''Creacion de gasto per capita'''
        self.frame['rgasto'] =(self.frame['gashog2d'].astype(float) 
                             / self.frame['mieperho'].astype(float) / 12)
        return self
    
    def func_rdpto(self):
        ''' creacion de departamento'''
        self.frame['rubigeo'] = self.frame['ubigeo'].astype(str)
        self.frame.loc[self.frame['rubigeo'].str.len()==5, 'rubigeo'] = '0' + self.frame['rubigeo']
        self.frame['rdpto'] = self.frame['rubigeo'].str[0:2]
        return self 
    
    def func_rpobre(self):
        self.frame['rpobre']=np.where(self.frame['pobreza']==3,0,1)
        return self.frame
