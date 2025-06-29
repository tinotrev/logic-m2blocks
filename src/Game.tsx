import { useEffect, useState } from 'react';
import PengineClient, { PrologTerm } from './PengineClient';
import Board from './Board';
import Block from './Block';
import { delay } from './util';

export type Grid = (number | "-")[];

interface EffectTerm extends PrologTerm {
  functor: "effect";
  args: [Grid, EffectInfoTerm[]];
}

interface ComboTerm extends PrologTerm {
  functor: "combo";
  args: [number];
}

interface NewBlockTerm extends PrologTerm {
  functor: "newBlock";
  args: [number];
}

interface GravityTerm extends PrologTerm {
  functor: "gravity";
  args: [];
}

interface CleanupTerm extends PrologTerm {
  functor: "cleanup";
  args: [number[]];
}

interface NewMaxBlockTerm extends PrologTerm {
  functor: "newMaxBlock";
  args: [number];
}

interface NewBlockAddedTerm extends PrologTerm {
  functor: "newBlockAdded";
  args: [];
}

interface BlockEliminatedTerm extends PrologTerm {
  functor: "blockEliminated";
  args: [];
}

interface ScoreTerm extends PrologTerm {
  functor: "score";
  args: [number];
}

type EffectInfoTerm = 
  | NewBlockTerm 
  | ComboTerm 
  | ScoreTerm
  | GravityTerm 
  | CleanupTerm 
  | NewMaxBlockTerm 
  | NewBlockAddedTerm 
  | BlockEliminatedTerm 
  | PrologTerm;

function Game() {
  const [pengine, setPengine] = useState<any>(null);

  const [grid, setGrid] = useState<Grid | null>(null);
  const [numOfColumns, setNumOfColumns] = useState<number | null>(null);

  const [waiting, setWaiting] = useState<boolean>(false);

  const [gameState, setGameState] = useState<any>(null);
  const [score, setScore] = useState<number>(0);
  const [shootBlock, setShootBlock] = useState<number | null>(null);
  const [nextBlock, setNextBlock] = useState<number | null>(null);
  const [showNextBlock, setShowNextBlock] = useState<boolean>(false);
  const [maxBlock, setMaxBlock] = useState<number>(0);

  const [showHints, setShowHints] = useState<boolean>(false);
  const [hintInfo, setHintInfo] = useState<{[column: number]: any}>({})

  const [notificationQueue, setNotificationQueue] = useState<string[]>([]);
  const [currentNotification, setCurrentNotification] = useState<string | null>(null);

  useEffect(() => {
    connectToPenginesServer();
  }, []);

  useEffect(() => {
    if (pengine) {
      initGame();
    }
  }, [pengine]);

  async function connectToPenginesServer() {
    setPengine(await PengineClient.create()); // Await until the server is initialized
  }

  async function initGame() {
    const initQueryS = 'init(Grid, NumOfColumns)';
    const initResponse = await pengine!.query(initQueryS);
    
    const gridS = JSON.stringify(initResponse['Grid']).replace(/"/g, '');
    const gameStateQueryS = `init_game_state(${gridS}, ${initResponse['NumOfColumns']}, GameState)`;
    
    const gameStateResponse = await pengine!.query(gameStateQueryS);
    
    if (gameStateResponse && gameStateResponse['GameState']) {
      const gameState = gameStateResponse['GameState'];
      
      const grid = gameState.args[0];
      const numCols = gameState.args[1];
      const currentBlock = gameState.args[2];
      const nextBlock = gameState.args[3];
      
      setGrid(grid);
      setNumOfColumns(numCols);
      setShootBlock(currentBlock);
      setNextBlock(nextBlock);
      setGameState(gameState);
      
      const initialMax = Math.max(...grid.filter((cell: any) => typeof cell === 'number'));
      setMaxBlock(initialMax);
    }
  }

  useEffect(() => {
    if (!currentNotification && notificationQueue.length > 0) {
      const nextNotification = notificationQueue[0];
      setCurrentNotification(nextNotification);
      setNotificationQueue(prev => prev.slice(1));
      
      // Ocultar después de 3 segundos
      setTimeout(() => {
        setCurrentNotification(null);
      }, 3000);
    }
  }, [currentNotification, notificationQueue]);

  async function handleLaneClick(lane: number) {
  if (waiting || !shootBlock || !gameState) return;

  setWaiting(true);

  try {
    const gameStateString = `gameState(${JSON.stringify(grid).replace(/"/g, '')}, ${numOfColumns}, ${shootBlock}, ${nextBlock})`;
    const shootQueryS = `shoot(${shootBlock}, ${lane}, ${JSON.stringify(grid).replace(/"/g, '')}, ${numOfColumns}, Effects)`;
    
    const [gameStateResponse, shootResponse] = await Promise.all([
      pengine.query(`update_game_state(${gameStateString}, ${lane}, NewGameState)`),
      pengine.query(shootQueryS)
    ]);

    if (gameStateResponse?.['NewGameState'] && shootResponse?.['Effects']) {
      const newGameState = gameStateResponse['NewGameState'];
      const effects = shootResponse['Effects'];
      
      const firstEffect = effects[0];
      const firstEffectGrid = firstEffect.args[0];
      setGrid(firstEffectGrid);
      
      const finalCurrentBlock = newGameState.args[2];
      const finalNextBlock = newGameState.args[3];
      setShootBlock(finalCurrentBlock);
      setNextBlock(finalNextBlock);
      setGameState(newGameState);
      
      const firstEffectInfo = firstEffect.args[1];
      firstEffectInfo.forEach((effectInfoItem: EffectInfoTerm) => {
        const { functor, args } = effectInfoItem;
        switch (functor) {
          case 'score':
            setScore(score => score + args[0]);
            break;
          case 'newBlock':
            console.log(`🎯 Nuevo bloque creado: ${args[0]}`);
            break;
        }
      });
      
      const remainingEffects = effects.slice(1);
      if (remainingEffects.length > 0) {
        await delay(1000); // Delay antes de continuar con el resto
        await animateRemainingEffects(remainingEffects);
      }
      
      const newMax = Math.max(...firstEffectGrid.filter((cell: any) => typeof cell === 'number'));
      if (newMax > maxBlock) {
        setMaxBlock(newMax);
      }
      
      setWaiting(false);
    } else {
      setWaiting(false);
    }
  } catch (error) {
    console.error('Error in lane click:', error);
    setWaiting(false);
  }
}

const addNotification = (message: string) => {
      setNotificationQueue(prev => [...prev, message]);
    };

async function animateRemainingEffects(effects: EffectTerm[]) {
  if (effects.length === 0) return;
  
  const effect = effects[0];
  const [effectGrid, effectInfo] = effect.args;
  setGrid(effectGrid);

  effectInfo.forEach((effectInfoItem) => {
    const { functor, args } = effectInfoItem;
    
    switch (functor) {
      case 'score':
        setScore(score => score + args[0]);
        break;
      case 'combo':
        if (args[0] >= 3) {
          addNotification(`Combo x${args[0]}!`);
        }
        break;
      case 'cleanup':
        addNotification(`Bloques limpiados: ${args[0].join(', ')}`);
        break;
      case 'newMaxBlock':
        if (args[0] >= 512) {
          addNotification(`¡Nuevo máximo desbloqueado: ${args[0]}!`);
        }
        break;
      case 'blockEliminated':
        addNotification('Bloques retirados del rango');
        break;
      case 'newBlockAdded':
        if (args && args[0] && args[0].length > 0) {
            const newBlocks = args[0].join(', ');
            addNotification(`¡Bloque ${newBlocks} desbloqueado para disparar!`);
        } else {
            addNotification('¡Nuevos bloques desbloqueados para disparar!');
        }
        break;
      default:
        break;
    }
  });

  const restEffects = effects.slice(1);
  if (restEffects.length > 0) {
    const hasGravity = effectInfo.some(info => info.functor === 'gravity');
    const delayTime = hasGravity ? 800 : 1000;
    
    await delay(delayTime);
    await animateRemainingEffects(restEffects);
  }
}
  async function getHintForColumn(column: number) {
    if (!pengine || !grid || !shootBlock) return null;
    
    const gridS = JSON.stringify(grid).replace(/"/g, '');
    const queryS = `hint_shot(${shootBlock}, ${column}, ${gridS}, ${numOfColumns}, HintInfo)`;
    
    try {
      const response = await pengine.query(queryS);
      return response ? response['HintInfo'] : null;
    } catch (error) {
      console.error('Error getting hint:', error);
      return null;
    }
  }

  async function loadAllHints() {
    if (!showHints || !numOfColumns) return;
    
    const hints: {[column: number]: any} = {};
    
    for (let col = 1; col <= numOfColumns; col++) {
      const hint = await getHintForColumn(col);
      hints[col] = hint;
    }
    
    setHintInfo(hints);
  }

  useEffect(() => {
    if (showHints) {
      loadAllHints();
    } else {
      setHintInfo({});
    }
  }, [showHints, grid, shootBlock]);

  if (grid === null) {
    return null;
  }
  
  return (
    <div className="game">      
      <div className="header">
        <div className="score">Score: {score}</div>
      </div>
      
      <div style={{ display: 'flex', gap: '10px', margin: '10px' }}>
        <button
          onClick={() => setShowHints(prev => !prev)}
          style={{
            padding: '10px 20px',
            fontSize: '16px',
            cursor: 'pointer',
            backgroundColor: showHints ? '#444' : '#222',
            color: 'white',
            border: '1px solid gray',
            borderRadius: '5px'
          }}
        >
          {showHints ? 'Ocultar Hints' : 'Mostrar Hints'}
        </button>

        <button
          onClick={() => setShowNextBlock(prev => !prev)}
          style={{
            padding: '10px 20px',
            fontSize: '16px',
            cursor: 'pointer',
            backgroundColor: showNextBlock ? '#444' : '#222',
            color: 'white',
            border: '1px solid gray',
            borderRadius: '5px'
          }}
        >
          {showNextBlock ? 'Ocultar Siguiente' : 'Mostrar Siguiente Bloque'}
        </button>
      </div>

      <Board
        grid={grid}
        numOfColumns={numOfColumns!}
        onLaneClick={handleLaneClick}
        showHints={showHints}
        shblock={shootBlock!}
        nextBlock={nextBlock}
        hintInfo={hintInfo}
      />

      {currentNotification && (
        <div className="unified-notification-display">
          {currentNotification}
        </div>
      )}

      <div className='footer' style={{ position: 'relative' }}>
        <div className='blockShoot'>
          {shootBlock && <Block value={shootBlock} position={[0, 0]} />}
        </div>
        {showNextBlock && nextBlock && (
          <div style={{ 
            position: 'absolute',
            left: '50%',
            marginLeft: '100px',
            top: '50%',
            transform: 'translateY(-50%)',
            display: 'flex', 
            alignItems: 'center',
            color: 'white',
            fontSize: '25px'
          }}>
            <span style={{ marginRight: '10px' }}>Siguiente:</span>
            <div className='blockShoot' style={{ margin: 0 }}>
              <Block value={nextBlock} position={[0, 0]} />
            </div>
          </div>
        )}
      </div>
    </div>
  );
}

export default Game;