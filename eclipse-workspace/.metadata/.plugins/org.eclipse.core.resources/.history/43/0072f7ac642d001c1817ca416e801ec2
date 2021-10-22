package assignment1.testing;

import org.junit.*;

import assignment1.Board;
import assignment1.Cell;
import assignment1.Coordinate;

public class BoardTest {

    private Board board;

    @Before
    public void setup() {
        this.board = new Board();
    }

    @Test
    public void testGetCell() {
        Cell cell = board.getCell(new Coordinate(1, 4));
        Assert.assertNotNull(cell.getPiece());
    }

}
