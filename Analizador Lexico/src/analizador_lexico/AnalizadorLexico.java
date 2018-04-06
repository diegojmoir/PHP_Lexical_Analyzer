/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package analizador_lexico;

import java.io.File;

/**
 *
 * @author Diego Pérez Moir
 */
public class AnalizadorLexico {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        // TODO code application logic here
         
        String path = new File(".").getAbsolutePath();
        path = path.substring(0, path.length()-1) + "/src" + "/analizador_lexico" + "/PHP.flex";
        GenerateJflex(path); 
        
    }
        
     public static void GenerateJflex(String path){
        File file = new File(path);
        jflex.Main.generate(file);
     }
     
   
    
}
