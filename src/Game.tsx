import { useEffect, useState } from 'react';
import PengineClient, { PrologTerm } from './PengineClient';
import Board from './Board';
import Block from './Block';
import { delay } from './util';

export type Grid = (number | "-")[]; // Aquí se define el tipo Grid, que representa la grilla del juego. Es un arreglo cuyos elementos pueden ser números (los valores de los bloques) o el carácter "-", que indica una celda vacía.

// El tipo EffectTerm representa un término Prolog que contiene información sobre los efectos que ocurren en el juego. Tiene un functor "effect" y dos argumentos: la grilla resultante y una lista de efectos.
interface EffectTerm extends PrologTerm {
  functor: "effect";
  args: [Grid, EffectInfoTerm[]];
}

// Agregar nuevo tipo para combo
interface ComboTerm extends PrologTerm {
  functor: "combo";
  args: [number];
}

// NewBlockTerm es un término Prolog que representa la creación de un nuevo bloque en el juego. Tiene un functor "newBlock" y un argumento que indica el valor del nuevo bloque.
interface NewBlockTerm extends PrologTerm {
  functor: "newBlock";
  args: [number];
}

// Nuevo tipo para gravedad
interface GravityTerm extends PrologTerm {
  functor: "gravity";
  args: [];
}

// Nuevo tipo para limpieza de bloques
interface CleanupTerm extends PrologTerm {
  functor: "cleanup";
  args: [number[]];
}

// Nuevo tipo para nuevo máximo
interface NewMaxBlockTerm extends PrologTerm {
  functor: "newMaxBlock";
  args: [number];
}

// Nuevo tipo para bloque agregado
interface NewBlockAddedTerm extends PrologTerm {
  functor: "newBlockAdded";
  args: [];
}

// Nuevo tipo para bloque eliminado
interface BlockEliminatedTerm extends PrologTerm {
  functor: "blockEliminated";
  args: [];
}

// Nuevo tipo para score
interface ScoreTerm extends PrologTerm {
  functor: "score";
  args: [number];
}

// Aquí se define el tipo EffectInfoTerm, que representa la información de los efectos que ocurren en el juego.
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
  

  // State
  const [pengine, setPengine] = useState<any>(null);
  const [grid, setGrid] = useState<Grid | null>(null);
  const [numOfColumns, setNumOfColumns] = useState<number | null>(null);
  const [score, setScore] = useState<number>(0);
  const [shootBlock, setShootBlock] = useState<number | null>(null);
  const [waiting, setWaiting] = useState<boolean>(false);

  // Agregar estado para combo
  const [currentCombo, setCurrentCombo] = useState<number>(0);
  const [showCombo, setShowCombo] = useState<boolean>(false);

  // Nuevos estados para las funcionalidades agregadas
  const [maxBlock, setMaxBlock] = useState<number>(0);
  const [showNewMax, setShowNewMax] = useState<boolean>(false);
  const [showNotification, setShowNotification] = useState<string>('');
  const [cleanedBlocks, setCleanedBlocks] = useState<number[]>([]);
  const [showCleanup, setShowCleanup] = useState<boolean>(false);
  const [showHints, setShowHints] = useState<boolean>(false);
  const [showNextBlock, setShowNextBlock] = useState<boolean>(false);

  // Nuevo estado para hints
  const [hintInfo, setHintInfo] = useState<{[column: number]: any}>({})

  const [gameState, setGameState] = useState<any>(null);
  const [nextBlock, setNextBlock] = useState<number | null>(null);

    // Nuevo sistema de notificaciones secuenciales
  const [notificationQueue, setNotificationQueue] = useState<string[]>([]);
  const [currentNotification, setCurrentNotification] = useState<string | null>(null);

  // Función para agregar notificación a la cola
  const addNotification = (message: string) => {
    setNotificationQueue(prev => [...prev, message]);
  };

  useEffect(() => {
    // This is executed just once, after the first render.
    connectToPenginesServer();
  }, []);

  useEffect(() => {
    if (pengine) {
      // This is executed after pengine was set.
      initGame();
    }
  }, [pengine]);

  async function connectToPenginesServer() {
    setPengine(await PengineClient.create()); // Await until the server is initialized
  }

  async function initGame() {
    // Primero inicializar con el sistema tradicional para obtener la grilla base
    const initQueryS = 'init(Grid, NumOfColumns)';
    const initResponse = await pengine!.query(initQueryS);
    
    // Ahora inicializar el estado del juego con bloques actual/siguiente
    const gridS = JSON.stringify(initResponse['Grid']).replace(/"/g, '');
    const gameStateQueryS = `init_game_state(${gridS}, ${initResponse['NumOfColumns']}, GameState)`;
    
    const gameStateResponse = await pengine!.query(gameStateQueryS);
    
    if (gameStateResponse && gameStateResponse['GameState']) {
      const gameState = gameStateResponse['GameState'];
      
      // Extraer información del estado del juego
      const grid = gameState.args[0];
      const numCols = gameState.args[1];
      const currentBlock = gameState.args[2];
      const nextBlock = gameState.args[3];
      
      // Actualizar todos los estados
      setGrid(grid);
      setNumOfColumns(numCols);
      setShootBlock(currentBlock);
      setNextBlock(nextBlock);
      setGameState(gameState);
      
      // Calcular el máximo inicial
      const initialMax = Math.max(...grid.filter((cell: any) => typeof cell === 'number'));
      setMaxBlock(initialMax);
    }
  }

  // Effect para procesar la cola de notificaciones
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


  /**
   * Called when the player clicks on a lane.
   */
  // Versión corregida que actualiza inmediatamente los bloques del footer
  async function handleLaneClick(lane: number) {
  if (waiting || !shootBlock || !gameState) return;

  setWaiting(true);

  try {
    // PASO 1: Hacer el disparo y obtener efectos
    const gameStateString = `gameState(${JSON.stringify(grid).replace(/"/g, '')}, ${numOfColumns}, ${shootBlock}, ${nextBlock})`;
    const shootQueryS = `shoot(${shootBlock}, ${lane}, ${JSON.stringify(grid).replace(/"/g, '')}, ${numOfColumns}, Effects)`;
    
    const [gameStateResponse, shootResponse] = await Promise.all([
      pengine.query(`update_game_state(${gameStateString}, ${lane}, NewGameState)`),
      pengine.query(shootQueryS)
    ]);

    if (gameStateResponse?.['NewGameState'] && shootResponse?.['Effects']) {
      const newGameState = gameStateResponse['NewGameState'];
      const effects = shootResponse['Effects'];
      
      // PASO 2: Actualizar INMEDIATAMENTE la primera grilla de los efectos
      // Esto hace que el bloque aparezca en la grilla sin delay
      const firstEffect = effects[0];
      const firstEffectGrid = firstEffect.args[0];
      setGrid(firstEffectGrid);
      
      // PASO 3: Actualizar los bloques del footer
      const finalCurrentBlock = newGameState.args[2];
      const finalNextBlock = newGameState.args[3];
      setShootBlock(finalCurrentBlock);
      setNextBlock(finalNextBlock);
      setGameState(newGameState);
      
      // PASO 4: Procesar efectos del primer paso (sin cambiar la grilla)
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
          // ... otros efectos
        }
      });
      
      // PASO 5: Animar el resto de efectos si existen
      const remainingEffects = effects.slice(1);
      if (remainingEffects.length > 0) {
        await delay(1000); // Delay antes de continuar con el resto
        await animateRemainingEffects(remainingEffects);
      }
      
      // Actualizar máximo al final
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

// Nueva función auxiliar para animar solo los efectos restantes
async function animateRemainingEffects(effects: EffectTerm[]) {
  if (effects.length === 0) return;
  
  const effect = effects[0];
  const [effectGrid, effectInfo] = effect.args;
  setGrid(effectGrid);

  // Procesar efectos
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

  // Nueva función que maneja las animaciones y actualiza la UI al final
  async function animateEffectsWithFinalUpdate(
    effects: EffectTerm[], 
    finalGrid: Grid, 
    newCurrentBlock: number, 
    newNextBlock: number, 
    newGameState: any
  ) {
    // Animar todos los efectos SIN cambiar los bloques de disparo
    await animateEffectsOnly(effects);
    
    // SOLO al final, actualizar toda la UI de una vez
    updateGameStateUI(finalGrid, newCurrentBlock, newNextBlock, newGameState);
    setWaiting(false);
  }

  // Función helper para actualizar toda la UI de una vez
  function updateGameStateUI(
    newGrid: Grid, 
    newCurrentBlock: number, 
    newNextBlock: number, 
    newGameState: any
  ) {
    setGrid(newGrid);
    setShootBlock(newCurrentBlock);
    setNextBlock(newNextBlock);
    setGameState(newGameState);
  }

  // Versión mejorada de animateEffectsOnly que NO toca los bloques de disparo
  async function animateEffectsOnly(effects: EffectTerm[]) {
    const effect = effects[0];    
    const [effectGrid, effectInfo] = effect.args;
    
    // Solo actualiza la grilla, NO los bloques de disparo
    setGrid(effectGrid);

    // Procesar efectos (score, combos, etc.)
    effectInfo.forEach((effectInfoItem) => {
      const { functor, args } = effectInfoItem;
      
      // DEBUG: Mostrar todos los efectos que llegan
      console.log(`🔔 Procesando efecto: ${functor}`, args);
      
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
          // Solo notificar si es >= 512
          if (args[0] >= 512) {
            addNotification(`¡Nuevo máximo desbloqueado: ${args[0]}!`);
          }
          break;
        case 'blockEliminated':
          addNotification('Bloques retirados del rango');
          break;
        case 'newBlockAdded':
          console.log('🎯 newBlockAdded detectado!');
          addNotification('Nuevos bloques agregados al rango');
          break;
        default:
          console.log(`⚠️ Efecto no manejado: ${functor}`, args);
          break;
      }
    });

    const restEffects = effects.slice(1);
    if (restEffects.length === 0) {
      // No más efectos, pero NO cambiar waiting aquí
      // Se cambiará en animateEffectsWithFinalUpdate
      return;
    }
    
    const hasGravity = effectInfo.some(info => info.functor === 'gravity');
    const delayTime = hasGravity ? 800 : 1000;
    
    await delay(delayTime);
    await animateEffectsOnly(restEffects);
  }

  // Función alternativa más simple si prefieres mantener la lógica original
  // pero con validación adicional
  async function handleLaneClickWithValidation(lane: number) {
    if (waiting || !shootBlock) return;

    setWaiting(true);
    
    try {
      // Hacer el disparo normal
      const queryS = `shoot(${shootBlock}, ${lane}, ${JSON.stringify(grid).replace(/"/g, '')}, ${numOfColumns}, Effects)`;
      const response = await pengine.query(queryS);
      
      if (response && response['Effects']) {
        animateEffect(response['Effects']);
        
        const lastEffect = response['Effects'][response['Effects'].length - 1];
        const finalGrid = lastEffect.args[0];
        
        // Verificar si el nextBlock sigue siendo válido después del disparo
        const validationQueryS = `validate_block_in_range(${nextBlock}, ${JSON.stringify(finalGrid).replace(/"/g, '')})`;
        
        try {
          await pengine.query(validationQueryS);
          // nextBlock es válido, usar flujo normal
          setShootBlock(nextBlock);
          
          const newNextBlockQueryS = `randomBlock(${JSON.stringify(finalGrid).replace(/"/g, '')}, NewBlock)`;
          const newNextBlockResponse = await pengine.query(newNextBlockQueryS);
          
          if (newNextBlockResponse && newNextBlockResponse['NewBlock']) {
            setNextBlock(newNextBlockResponse['NewBlock']);
          }
        } catch (error) {
          // nextBlock no es válido, generar ambos bloques nuevos
          // Removida notificación no deseada
          
          const newPairQueryS = `generate_block_pair(${JSON.stringify(finalGrid).replace(/"/g, '')}, NewCurrent, NewNext)`;
          const newPairResponse = await pengine.query(newPairQueryS);
          
          if (newPairResponse && newPairResponse['NewCurrent'] && newPairResponse['NewNext']) {
            setShootBlock(newPairResponse['NewCurrent']);
            setNextBlock(newPairResponse['NewNext']);
          }
        }
      } else {
        setWaiting(false);
      }
    } catch (error) {
      console.error('Error in lane click:', error);
      setWaiting(false);
    }
  }
  
  /**
   * Displays each grid of the sequence as the current grid in 1sec intervals, and considers the other effect information.
   * @param effects The list of effects to be animated.
   */
  async function animateEffect(effects: EffectTerm[]) {
    const effect = effects[0];    
    const [effectGrid, effectInfo] = effect.args;
    setGrid(effectGrid);

    // DEBUG: Ver qué efectos están llegando
    console.log('🔥 Efectos en este step:', effectInfo.map(e => e.functor));
    console.log('🔥 Efectos completos:', effectInfo);    // Procesar todos los efectos de este step
     effectInfo.forEach((effectInfoItem) => {
        const { functor, args } = effectInfoItem;
        
        switch (functor) {
            case 'score':
                // CORREGIDO: El score real viene del efecto 'score', no de 'newBlock'
                setScore(score => score + args[0]);
                break;
            case 'newBlock':
                // Este es solo el valor del nuevo bloque creado, no el score
                console.log(`🎯 Nuevo bloque creado: ${args[0]}`);
                break;
            case 'combo':
                if (args[0] >= 3) {
                    addNotification(`Combo x${args[0]}!`);
                }
                break;
            case 'cleanup':
                addNotification(`Bloque eliminado: ${args[0].join(', ')}`);
                setCleanedBlocks(args[0]);
                break;
            case 'newMaxBlock':
                // Solo notificar si es >= 512
                if (args[0] >= 512) {
                    addNotification(`¡Nuevo máximo desbloqueado: ${args[0]}!`);
                }
                setMaxBlock(args[0]);
                break;
            case 'blockEliminated':
                addNotification('Bloques retirados del rango');
                break;
            case 'newBlockAdded':
                addNotification('Nuevos bloques agregados al rango');
                break;
            default:
                break;
        }
    });


    const restRGrids = effects.slice(1);
    if (restRGrids.length === 0) {
      setWaiting(false);
      return;
    }
    
    // Ajustar el delay según el tipo de efecto
    const hasGravity = effectInfo.some(info => info.functor === 'gravity');
    const delayTime = hasGravity ? 800 : 1000; // Gravedad más rápida
    
    await delay(delayTime);
    animateEffect(restRGrids);
  }
  

  // Modificar getHintForColumn para usar el estado del juego
  // Nueva función para obtener hint de una columna específica
  async function getHintForColumn(column: number) {
    if (!pengine || !grid || !shootBlock) return null;
    
    // Usar el formato original: hint_shot(Block, Column, Grid, NumOfColumns, HintInfo)
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

    // Función para obtener hints de todas las columnas cuando se activa showHints
  async function loadAllHints() {
    if (!showHints || !numOfColumns) return;
    
    const hints: {[column: number]: any} = {};
    
    for (let col = 1; col <= numOfColumns; col++) {
      const hint = await getHintForColumn(col);
      hints[col] = hint;
    }
    
    setHintInfo(hints);
  }

  // Effect para cargar hints cuando se activa showHints
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
        <div className="max-block">Max: {maxBlock}</div>
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
        nextBlock={nextBlock} // Agregar esta línea
        hintInfo={hintInfo} // Pasar la información de hints
      />

      {/* Sistema de notificaciones unificado */}
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