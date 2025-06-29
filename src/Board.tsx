import Block, { Position } from './Block';
import { Grid } from './Game';
import { useState } from 'react';

interface BoardProps {
  grid: Grid;
  numOfColumns: number;
  onLaneClick: (lane: number) => void;
  showHints: boolean;
  shblock: number | null;
  nextBlock: number | null;
  hintInfo: {[column: number]: any};
}

function Board({ grid, numOfColumns, onLaneClick, showHints, shblock, hintInfo, nextBlock }: BoardProps) {
  const numOfRows = grid.length / numOfColumns;
  const [hoveredLane, setHoveredLane] = useState<number | null>(null);
  const [mousePosition, setMousePosition] = useState({ x: 0, y: 0 });

  const handleMouseMove = (e: React.MouseEvent) => {
    setMousePosition({ x: e.clientX, y: e.clientY });
  };
  
  const formatHintInfo = (hint: any[]): string => {
    if (!hint || hint.length === 0) return 'Sin información';
    
    const hintTypes = hint
      .filter(item => {
        if (typeof item === 'object') {
          if (item.functor === 'nextBlock' || item.functor === 'newBlock') {
            return false;
          }
        }
        return true;
      })
      .map(item => {
        if (typeof item === 'string') {
          switch(item) {
            case 'no_shot': return 'No se puede disparar';
            case 'no_combo': return 'Sin combo';
            case 'simple_combo': return 'Combo simple';
            default: return item;
          }
        } else if (typeof item === 'object' && item.functor) {
          switch(item.functor) {
            case 'combo':
              return `Combo x${item.args[0]}`;
            case 'score':
              return `Score: ${item.args[0]}`;
            default:
              return null;
          }
        }
        return String(item);
      })
      .filter(item => item !== null);
    
    return hintTypes.join(', ');
  };


  return (
    <div className="board" style={{ position: 'relative' }}>
      <div
        className="blocks"
        style={{
          display: 'grid',
          gridTemplateColumns: `repeat(${numOfColumns}, 70px)`,
          gridTemplateRows: `repeat(${numOfRows}, 70px)`,
        }}
      >
        {grid.map((num, i) => {
          if (num === '-') return null;
          const pos: Position = [Math.floor(i / numOfColumns), i % numOfColumns];
          return <Block value={num} position={pos} key={i} />;
        })}

        {Array.from({ length: numOfColumns }).map((_, i) => (
          <div
            className="lane"
            style={{
              gridColumn: i + 1,
              gridRow: `1 / span ${numOfRows}`,
            }}
            onClick={() => onLaneClick(i + 1)}
            onMouseEnter={() => setHoveredLane(i)}
            onMouseLeave={() => setHoveredLane(null)}
            onMouseMove={handleMouseMove}
            key={`lane-${i}`}
          />
        ))}
      </div>
      {showHints && hoveredLane !== null && (
        <div
          className="tooltip"
          style={{
            position: 'fixed',
            left: mousePosition.x + 10,
            top: mousePosition.y - 10,
            pointerEvents: 'none',
          }}
        >
          <div style={{ display: 'flex', alignItems: 'center', gap: '8px', marginBottom: '8px' }}>
            Actual:
            <div className="tooltip-block" style={{ fontSize: '10px' }}>
              <Block value={shblock!} position={[0, 0]} />
            </div>
          </div>
          
          {formatHintInfo(hintInfo[hoveredLane + 1])}
        </div>
      )}
    </div>
  );
}

export default Board;