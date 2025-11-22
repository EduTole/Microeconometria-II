import pandas as pd
import numpy as np

# Codigos de ENAHO : modulo 500
class process500(object):
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
    
    def func_rincome(self):
        
        variables_income = ['i524a1', 'd529t', 'i530a', \
                            'd536', 'i538a1', 'd540t', \
                                'i541a', 'd543', 'd544t']
        base_income = self.frame[variables_income]
        self.frame['r6'] = np.where(self.frame['ocu500']==1,\
                                    base_income.sum(axis=1),0 )
        self.frame['r6'] = self.frame['r6']/12
            
        return self
    
    def func_rage(self):
        self.frame['redad'] = pd.to_numeric(self.frame['p208a'])
        return self
    
    def func_mujer(self):
        self.frame['rmujer'] =np.where(self.frame['p207']==2,1,0)
        return self
    
    def func_rdpto(self):
        ''' creacion de departamento'''
        self.frame['rubigeo'] = self.frame['ubigeo'].astype(str)
        self.frame.loc[self.frame['rubigeo'].str.len()==5, \
                       'rubigeo'] = '0' + self.frame['rubigeo']
        self.frame['rdpto'] = self.frame['rubigeo'].str[0:2]
        self.frame['rDpto'] = self.frame['rdpto'].replace(
            {"01": "Amazonas","02": "Ancash","03": "Apurimac","04":"Arequipa","05": "Ayacucho",
             "06": "Cajamarca","07": "Callao","08": "Cusco","09": "Huancavelica","10":"Huanuco",
             "11": "Ica","12":"Junin", "13": "La Libertad","14": "Lambayeque","15":"Lima",
             "16": "Loreto","17":"Madre de Dios","18": "Moquegua","19":"Pasco","20":"Piura",
             "21": "Puno","22":"San Martin","23":"Tacna","24":"Tumbes","25":"Ucayali"
             }
            )
        
        return self 
    
    def func_rocu(self):
       self.frame['rocu'] = np.where(self.frame['ocu500']==1, 1,
                                  np.where(self.frame['ocu500']==2,2, 3))
       return self 
    
    def func_desemp(self):
        self.frame['rmu'] = np.where(self.frame['ocu500']==2,1,0)
        return self
    
    def func_educa(self):
        p = self.frame['p301a']
        self.frame['reduca'] = np.where(p == 1, 1,
                            np.where((p >= 2) & (p <= 4), 2,
                            np.where((p >= 5) & (p <= 6), 3,
                            np.where((p >= 7) & (p <= 8), 4,
                            np.where((p >= 9) & (p <= 11), 5, 6)))))

        return self     
    def func_informal(self):
        self.frame['rinformal']=np.where(self.frame['ocupinf']==1,1,0)
        return self.frame 
