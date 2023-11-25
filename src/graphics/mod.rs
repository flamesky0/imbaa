use std::fs::File;

pub enum Cell {
    Empty,
    Wall,
    Box(u32), // Number of boxes
    Start, // of the maze
    Finish
}

struct Robot {
    x : u32,
    y : u32,
    lifting_capacity : u32,
    /* etc */
}
pub struct Map {
    file : File, // file to read map from

    /* array with enums Cell */
    robot : Robot, // coordinates
}

impl Map {
    pub fn new(file : File) {
        /* constructor - file open, read map, initialize structures */
    }
    pub fn robot_forward(num : u64) -> bool {
        todo!()
    }
    pub fn robot_backward(num : u64) -> bool {
        todo!()
    }
    pub fn robot_turn_left() -> bool {
        todo!()
    }
    pub fn robot_turn_right() -> bool {
        todo!()
    }
    pub fn load_boxes(num : u64) -> () {
        /* always succedes */
        todo!()
    }
    pub fn drop_boxes(num : u64) -> () {
        /* always succedes */
        todo!()
    }
    pub fn look() -> u64 {
        /* returns distance to an obstacle */
        todo!()
    }
    pub fn test() -> Cell {
        /* returns type of Cell in front of Robot */
        todo!()
    }
}

