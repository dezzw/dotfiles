package guifx;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import javafx.scene.control.Label;
import javafx.geometry.Insets;

public class HelloWorld extends Application {

	public static void main(String[] args) {
		
		/**
		 * static method of Application
		 * Creates an instance of Application, 
		 * starts the GUI thread and calls
		 * Application.start(stage) where stage is the window
		 */
		launch(args);
	}

	@Override
	public void start(Stage stage) {
		initUI(stage);
	}

	private void initUI(Stage stage) {

		/**
		 * A stage is the top level GUI window. 
		 * A stage has a scene. 
		 * A scene is a tree/graph of stuff, that is. Nodes in the scene are ...
		 * 
		 * LAYOUTS: which organize how its subtrees appear
		 * https://docs.oracle.com/javafx/2/layout/builtin_layouts.htm#CHDGHCDG
		 *
		 * CONTROLS: things which you interact with...
		 * https://docs.oracle.com/javafx/2/ui_controls/jfxpub-ui_controls.htm
		 * 
		 * EVENTS: Controls communicate with your code through callbacks. 
		 * Basically the Observable pattern.
		 *
		 */

		Label label = new Label("Hello World!!"); // CONTROL
		Label newLabel = new Label("CSC207");

		VBox root = new VBox(); // LAYOUT
		root.setPadding(new Insets(5));
		root.getChildren().add(label);
		root.getChildren().add(newLabel);
		
		Scene scene = new Scene(root, 280, 200); // SCENE
		
		stage.setTitle("Hello World JavaFX");
		stage.setScene(scene);
		stage.show();
	}
}

