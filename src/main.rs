use crate::Action::{EndTurn, MoveAction};
use cgmath::Vector2;
use std::collections::{HashMap, VecDeque};
use std::io;

/// Crime Time
/// A game where you control a single character at the start and end up building a criminal empire
/// World - Encompass every entities in the world
/// Chunk - Part of the world/city. It holds a small region in the world space
/// Entity - Any thing that exists in the world. Includes Human, Object, Item and Vehicles for now
/// Component - Part of an entity. It's optional.
///
/// Gameplay:
/// 1) A player gets to control each character.
/// 2) Each character has the same number of actions they can do per turn.
/// 3) After a character ends their turn, the AI gets a chance to do all available actions they can on their turn.
/// 4) Every action alters the world state. And each action consume a different number of action points.
/// 5) Every character has attributes and state.
/// 6) Attributes determined how viable they are at certain tasks (Example: Speed, Intelligence, Dexterity, Agility, Stamina)
/// 7) States can also be used to drive action and the outcome of actions (Example: Energy, Hunger, Will, Fear)
///
/// TODO: First stage of refactor. There are a lot of mixed data sharing and responsibilities which causes me to write hacky lifetimes. FIX IT!

type EntityId = uuid::Uuid;
type ChunkCoord = Vector2<i64>;
type CellCoord = Vector2<i32>;

const CHUNK_SIZE: i64 = 12;
const CHUNK_START: i64 = -(CHUNK_SIZE / 2);
const CHUNK_END: i64 = CHUNK_SIZE / 2;
const CELL_SIZE: i32 = 4; // 32

trait ToIndex {
    fn make_index(&self, width: i32) -> usize;
}

impl ToIndex for CellCoord {
    fn make_index(&self, width: i32) -> usize {
        (self.y * width + self.x) as usize
    }
}

#[derive(Copy, Clone)]
struct CoordinatePack {
    cell: CellCoord,
    chunk: ChunkCoord,
}

impl CoordinatePack {
    fn canonicalize(&self) -> CoordinatePack {
        let diff_x = if self.cell.x > -1 {
            (CELL_SIZE - 1) - self.cell.x
        } else {
            self.cell.x
        };
        let diff_y = if self.cell.y > -1 {
            (CELL_SIZE - 1) - self.cell.y
        } else {
            self.cell.y
        };

        if !(diff_x < 0 || diff_y < 0) {
            return *self;
        }

        let chunk_offset_x = self.cell.x / CELL_SIZE;
        let chunk_offset_y = self.cell.y / CELL_SIZE;
        let new_cell_x = self.cell.x % CELL_SIZE;
        let new_cell_y = self.cell.y % CELL_SIZE;
        let new_chunk_x = self.chunk.x + (chunk_offset_x as i64);
        let new_chunk_y = self.chunk.y + (chunk_offset_y as i64);

        CoordinatePack {
            cell: CellCoord::new(new_cell_x, new_cell_y),
            chunk: ChunkCoord::new(new_chunk_x, new_chunk_y),
        }
    }
}

impl Default for CoordinatePack {
    fn default() -> Self {
        CoordinatePack {
            cell: CellCoord::new(0, 0),
            chunk: ChunkCoord::new(0, 0),
        }
    }
}

pub struct World {
    humans: HashMap<EntityId, Human>,
    // TODO: Add different maps for Vehicles, Objects and Items
    chunks: HashMap<ChunkCoord, Chunk>,
}

impl World {
    fn new() -> Self {
        World {
            humans: HashMap::new(),
            chunks: HashMap::new(),
        }
    }

    fn human(&self, entity_id: EntityId) -> &Human {
        self.humans.get(&entity_id).unwrap()
    }

    fn human_mut(&mut self, entity_id: EntityId) -> &mut Human {
        self.humans.get_mut(&entity_id).unwrap()
    }

    fn create_human(&mut self, name: String) -> EntityId {
        let entity_id = uuid::Uuid::new_v4();

        self.humans.insert(entity_id, Human::new(entity_id, name));

        entity_id
    }

    fn add_human_to_chunk(&mut self, entity_id: EntityId, chunk_coord: ChunkCoord) {
        if let Some(chunk) = self.chunks.get_mut(&chunk_coord) {
            println!(
                "Adding human to chunk: [{},{}]",
                chunk_coord.x, chunk_coord.y
            );
            chunk.humans.push(entity_id);
            println!("Chunk Size: {}", chunk.humans.len());
        } else {
            println!(
                "Failed to get the chunk at [{},{}]",
                chunk_coord.x, chunk_coord.y
            );
        }
    }

    fn remove_human_from_chunk(&mut self, entity_id: EntityId, chunk_coord: ChunkCoord) {
        if let Some(chunk) = self.chunks.get_mut(&chunk_coord) {
            println!(
                "Removed human from chunk: [{},{}]",
                chunk_coord.x, chunk_coord.y
            );
            chunk.humans.retain(|other_id| entity_id != *other_id);
            println!("Chunk Size: {}", chunk.humans.len());
        } else {
            println!(
                "Failed to get the chunk at [{},{}]",
                chunk_coord.x, chunk_coord.y
            );
        }
    }

    fn spawn_human(&mut self, entity_id: EntityId, coord: CoordinatePack) {
        let entity = self.human_mut(entity_id);
        entity.coord = coord;

        self.add_human_to_chunk(entity_id, coord.chunk);
    }

    fn update_human_coord(&mut self, entity_id: EntityId) -> CoordinatePack {
        let entity = self.human_mut(entity_id);
        let old_coord = entity.coord;
        let new_coord = entity.coord.canonicalize();
        entity.coord = new_coord;

        if old_coord.chunk != new_coord.chunk {
            self.remove_human_from_chunk(entity_id, old_coord.chunk);
            self.add_human_to_chunk(entity_id, new_coord.chunk);
        }

        new_coord
    }
}

struct Chunk {
    coord: ChunkCoord,
    humans: Vec<EntityId>,
    cells: Vec<Cell>,
}

impl Chunk {
    fn new(coord: ChunkCoord) -> Self {
        Chunk {
            coord,
            humans: Vec::new(),
            cells: Vec::new(),
        }
    }
}

struct Cell {
    coord: CellCoord,
    kind: CellType,
}

impl Cell {
    fn new(coord: CellCoord, kind: CellType) -> Self {
        Cell { coord, kind }
    }
}

enum CellType {
    Grass,
    Road,
}

struct HumanAttrib {
    current: u16,
    experience: f32,
}

impl Default for HumanAttrib {
    fn default() -> Self {
        HumanAttrib {
            current: 0,
            experience: 0.0,
        }
    }
}

#[derive(Default)]
struct HumanAttributes {
    stamina: HumanAttrib,
    intelligence: HumanAttrib,
}

struct HumanState {
    current: f32,
    max: f32,
}

impl Default for HumanState {
    fn default() -> Self {
        HumanState {
            current: 0.0,
            max: 0.0,
        }
    }
}

#[derive(Default)]
struct HumanStates {
    health: HumanState,
    energy: HumanState,
    hunger: HumanState,
}

struct Human {
    id: EntityId,
    name: String,
    coord: CoordinatePack,
    attributes: HumanAttributes,
    states: HumanStates,
    // TODO: Optional component should be inside a box
}

impl Human {
    // TODO: Figure out a good default for chunk coord and cell coord
    fn new(id: EntityId, name: String) -> Self {
        Human {
            id,
            name,
            coord: CoordinatePack::default(),
            attributes: HumanAttributes::default(),
            states: HumanStates::default(),
        }
    }
}

#[derive(PartialEq)]
enum ControllerType {
    Player,
    AI,
}

struct Controller {
    kind: ControllerType,
    active_entity: EntityId, // TODO: This should also be a list of entities?
    controlled_entities: Vec<EntityId>,
    action_queue: ActionQueue,
}

impl Controller {
    fn new(kind: ControllerType, active_entity: EntityId) -> Self {
        Controller {
            kind,
            active_entity,
            controlled_entities: Vec::new(),
            action_queue: ActionQueue::new(),
        }
    }
}

struct ActionQueue {
    actions: VecDeque<Action>,
}

impl ActionQueue {
    fn new() -> Self {
        ActionQueue {
            actions: VecDeque::new(),
        }
    }
}

#[derive(Debug)]
enum MoveDirection {
    North,
    South,
    West,
    East,
}

impl MoveDirection {
    fn to_cell_coord(&self) -> CellCoord {
        match self {
            MoveDirection::North => CellCoord::new(0, 1),
            MoveDirection::South => CellCoord::new(0, -1),
            MoveDirection::West => CellCoord::new(-1, 0),
            MoveDirection::East => CellCoord::new(1, 0),
        }
    }
}

#[derive(Debug)]
enum Action {
    EndTurn,
    MoveAction { direction: MoveDirection },
}

impl Action {
    fn is_queueable(&self) -> bool {
        match self {
            Action::EndTurn => false,
            _ => true,
        }
    }

    fn is_end(&self) -> bool {
        match self {
            Action::EndTurn => true,
            _ => false,
        }
    }
}

struct Game {
    world: World,
    controllers: Vec<Controller>,
}

impl Game {
    fn new() -> Self {
        Game {
            world: World::new(),
            controllers: Vec::new(),
        }
    }

    fn start(&mut self) {
        println!("Game started!");

        world_generator::generate_world(&mut self.world);
    }

    fn add_controller(&mut self, controller: Controller) {
        self.controllers.push(controller)
    }
}

mod world_generator {
    use crate::{
        Cell, CellCoord, CellType, Chunk, ChunkCoord, ToIndex, World, CELL_SIZE, CHUNK_END,
        CHUNK_START,
    };

    pub fn generate_world(world: &mut World) {
        println!("Generating world...");

        generate_chunks(world);
        generate_entities(world);
    }

    fn generate_chunks(world: &mut World) {
        println!("Generating chunks...");

        for y in CHUNK_START..CHUNK_END {
            for x in CHUNK_START..CHUNK_END {
                let coord = ChunkCoord::new(x, y);

                world.chunks.insert(coord, Chunk::new(coord));

                if let Some(chunk) = world.chunks.get_mut(&coord) {
                    generate_cells(chunk)
                } else {
                    println!("Failed generating chunks: [{}, {}]", coord.x, coord.y);
                }
            }
        }
    }

    fn generate_entities(world: &mut World) {
        println!("Generating entities...");
    }

    fn generate_cells(chunk: &mut Chunk) {
        println!(
            "Generating cells for chunk: [{},{}]",
            chunk.coord.x, chunk.coord.y
        );

        for y in 0..CELL_SIZE {
            for x in 0..CELL_SIZE {
                let coord = CellCoord::new(x, y);
                let index = coord.make_index(CELL_SIZE);

                chunk.cells.insert(index, Cell::new(coord, CellType::Grass));
            }
        }
    }
}

fn update_controller(controller: &mut Controller, world: &mut World) {
    while let Some(action) = controller.action_queue.actions.pop_front() {
        execute_action(&action, controller.active_entity, world);
    }
}

fn generate_player_action(controller: &Controller, world: &World) -> Action {
    loop {
        println!("Please input your action: ");
        let mut action_buf = String::new();
        io::stdin().read_line(&mut action_buf).unwrap();
        let input = &action_buf.to_ascii_lowercase()[..];

        let action = if let Some(action) = create_player_action(input) {
            action
        } else {
            println!("Invalid action: {}", input);

            continue;
        };

        if !should_execute(&action, controller.active_entity, world) {
            println!("Cannot execute the action: {:?}", action);
            continue;
        }

        break action;
    }
}

fn generate_ai_action(controller: &Controller, world: &World) -> Action {
    Action::EndTurn
}

fn generate_controller_action(controller: &Controller, world: &World) -> Action {
    if controller.kind == ControllerType::Player {
        generate_player_action(controller, world)
    } else {
        generate_ai_action(controller, world)
    }
}

fn create_player_action(input: &str) -> Option<Action> {
    let args: Vec<&str> = input.trim().split(' ').collect();
    let command = args[0];

    match command {
        "move" => {
            println!("You moved!");
            Some(MoveAction {
                direction: MoveDirection::North,
            })
        }
        "end" => Some(EndTurn),
        _ => None,
    }
}

fn should_execute(action: &Action, entity_id: EntityId, world: &World) -> bool {
    match action {
        EndTurn => true,
        MoveAction { direction } => true,
    }
}

fn execute_action(action: &Action, entity_id: EntityId, world: &mut World) {
    match action {
        MoveAction { direction } => {
            let human = world.human_mut(entity_id);
            let human_id = human.id;
            human.coord.cell += direction.to_cell_coord();
            let new_coord = world.update_human_coord(human_id);

            println!(
                "Man you moved. Your loc is [{},{}]",
                new_coord.cell.x, new_coord.cell.y
            );
        }
        _ => {}
    }
}

fn main() {
    let mut game = Game::new();
    game.start();

    let entity_id = game.world.create_human(String::from("FeaR"));
    game.world.spawn_human(entity_id, CoordinatePack::default());

    game.add_controller(Controller::new(ControllerType::Player, entity_id));

    println!("Welcome to Crime Time!");
    println!();

    loop {
        for controller in &mut game.controllers {
            println!("It's your turn!");

            'action: loop {
                let action = generate_controller_action(controller, &game.world);

                if action.is_end() {
                    println!("END!");
                    update_controller(controller, &mut game.world);
                    break 'action;
                } else if action.is_queueable() {
                    println!("Added action to queue: {:?}", action);
                    controller.action_queue.actions.push_back(action);
                } else {
                    execute_action(&action, entity_id, &mut game.world)
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{CoordinatePack, CELL_SIZE};

    #[test]
    fn coord_canonicalize_same_chunk() {
        let mut coord = CoordinatePack::default();

        coord.cell.x = CELL_SIZE - 1;
        coord.cell.y = CELL_SIZE - 1;
        coord.chunk.x = 1;
        coord.chunk.y = 1;

        let mut canonicalize = coord.canonicalize();

        assert_eq!(canonicalize.cell.x, CELL_SIZE - 1);
        assert_eq!(canonicalize.cell.y, CELL_SIZE - 1);
        assert_eq!(canonicalize.chunk.x, 1);
        assert_eq!(canonicalize.chunk.y, 1);

        coord.cell.x = -(CELL_SIZE - 1);
        coord.cell.y = -(CELL_SIZE - 1);
        coord.chunk.x = 1;
        coord.chunk.y = 1;

        canonicalize = coord.canonicalize();

        assert_eq!(canonicalize.cell.x, -(CELL_SIZE - 1));
        assert_eq!(canonicalize.cell.y, -(CELL_SIZE - 1));
        assert_eq!(canonicalize.chunk.x, 1);
        assert_eq!(canonicalize.chunk.y, 1);
    }

    #[test]
    fn coord_canonicalize_chunk_1() {
        let mut coord = CoordinatePack::default();

        coord.cell.x = CELL_SIZE + 1;
        coord.cell.y = CELL_SIZE + 1;
        coord.chunk.x = 1;
        coord.chunk.y = 1;

        let mut canonicalize = coord.canonicalize();

        assert_eq!(canonicalize.cell.x, 1);
        assert_eq!(canonicalize.cell.y, 1);
        assert_eq!(canonicalize.chunk.x, 2);
        assert_eq!(canonicalize.chunk.y, 2);

        coord.cell.x = -(CELL_SIZE + 1);
        coord.cell.y = -(CELL_SIZE + 1);
        coord.chunk.x = 1;
        coord.chunk.y = 1;

        canonicalize = coord.canonicalize();

        assert_eq!(canonicalize.cell.x, -1);
        assert_eq!(canonicalize.cell.y, -1);
        assert_eq!(canonicalize.chunk.x, 0);
        assert_eq!(canonicalize.chunk.y, 0);
    }

    #[test]
    fn coord_canonicalize_chunk_3() {
        let mut coord = CoordinatePack::default();

        coord.cell.x = (CELL_SIZE * 3) + 1;
        coord.cell.y = (CELL_SIZE * 3) + 1;
        coord.chunk.x = 1;
        coord.chunk.y = 1;

        let mut canonicalize = coord.canonicalize();

        assert_eq!(canonicalize.cell.x, 1);
        assert_eq!(canonicalize.cell.y, 1);
        assert_eq!(canonicalize.chunk.x, 4);
        assert_eq!(canonicalize.chunk.y, 4);

        coord.cell.x = -((CELL_SIZE * 3) + 1);
        coord.cell.y = -((CELL_SIZE * 3) + 1);
        coord.chunk.x = 1;
        coord.chunk.y = 1;

        canonicalize = coord.canonicalize();

        assert_eq!(canonicalize.cell.x, -1);
        assert_eq!(canonicalize.cell.y, -1);
        assert_eq!(canonicalize.chunk.x, -2);
        assert_eq!(canonicalize.chunk.y, -2);
    }
}
