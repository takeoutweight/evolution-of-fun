package dk.itu.mario.engine;

import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.*;

import nathansorenson.LevelStub;

public class PlayCustomized {
	public static LevelStub generatedLevel;
	
	public static class ClosingListener extends WindowAdapter {
		MarioComponent m;
		ClosingListener(MarioComponent mc) {
			m = mc;
		}
    	public void windowClosing ( WindowEvent w ) {
    		m.stop();
    	} // end windowClosing
   	};
	
	public static MarioComponent runLevel(LevelStub level) 
	{
		generatedLevel = level;
		
		JFrame frame = new JFrame("Mario Experience Showcase");
		MarioComponent mario = new MarioComponent(640, 480,true);

    	frame.setContentPane(mario);
    	frame.setResizable(false);
        frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        frame.pack();

        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        frame.setLocation((screenSize.width-frame.getWidth())/2, (screenSize.height-frame.getHeight())/2);                

        frame.setVisible(true);
        frame.addWindowListener(new ClosingListener(mario));

        mario.start();      
        
        return mario;
	}
	
	public static void main(String[] args)
     {
		  runLevel(null); 
	}	

}
