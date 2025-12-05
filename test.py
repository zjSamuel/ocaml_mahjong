import sys
import copy

# --- 1. 基础定义与工具 ---

# 牌的编码索引 (0-33)
# 0-8: 1m-9m (万子)
# 9-17: 1p-9p (筒子)
# 18-26: 1s-9s (索子)
# 27-33: 东南西北白发中 (字牌)
TILES = [
    '1m', '2m', '3m', '4m', '5m', '6m', '7m', '8m', '9m',
    '1p', '2p', '3p', '4p', '5p', '6p', '7p', '8p', '9p',
    '1s', '2s', '3s', '4s', '5s', '6s', '7s', '8s', '9s',
    '东', '南', '西', '北', '白', '发', '中'
]

def str_to_index(tile_str):
    """将牌的字符串转换为索引"""
    if tile_str in TILES:
        return TILES.index(tile_str)
    # 处理类似 "1m" 的输入，如果是中文映射
    return -1

def tiles_to_counts(hand_str_list):
    """将手牌列表转换为34长度的频率数组"""
    counts = [0] * 34
    for t in hand_str_list:
        idx = str_to_index(t)
        if idx != -1:
            counts[idx] += 1
    return counts

def counts_to_tiles(counts):
    """(Debug用) 将counts还原为字符串列表"""
    res = []
    for i in range(34):
        for _ in range(counts[i]):
            res.append(TILES[i])
    return res

# --- 2. 核心算法：向听数计算 (Shanten) ---

def calculate_shanten(counts):
    """
    计算当前手牌的最小向听数。
    返回值：
    -1: 和牌
    0: 听牌
    1: 一向听
    ...
    """
    # 1. 计算国士无双向听
    shanten_kokushi = get_kokushi_shanten(counts)
    
    # 2. 计算七对子向听
    shanten_chiitoi = get_chiitoitsu_shanten(counts)
    
    # 3. 计算一般型（4面子1雀头）向听
    shanten_normal = get_normal_shanten(counts)
    
    return min(shanten_kokushi, shanten_chiitoi, shanten_normal)

def get_kokushi_shanten(counts):
    """国士无双：需要13种幺九牌各一张，其中一种有两张"""
    yaochu_indices = [0, 8, 9, 17, 18, 26, 27, 28, 29, 30, 31, 32, 33]
    types_count = 0
    has_pair = False
    
    for idx in yaochu_indices:
        if counts[idx] > 0:
            types_count += 1
            if counts[idx] >= 2:
                has_pair = True
                
    # 公式：13 - 幺九种类数 - (如果有幺九对子 ? 1 : 0)
    return 13 - types_count - (1 if has_pair else 0)

def get_chiitoitsu_shanten(counts):
    """七对子：需要7个不同的对子"""
    pairs = 0
    kinds = 0
    for c in counts:
        if c > 0:
            kinds += 1
        if c >= 2:
            pairs += 1
    
    # 向听数 = 6 - 对子数 + (如果种类不足7种造成的这种缺失，通常用 6 - pairs 即可，
    # 但如果手牌有3张或4张一样的，七对子依然只能算1个对子)
    # 另外需要处理手里只有 <7 种牌的情况 (极其罕见的一向听地狱，这里简化通用公式)
    shanten = 6 - pairs
    # 如果种类少于7种，必须补足种类，这里做一个简单的修正
    if kinds < 7:
        shanten += (7 - kinds)
    return shanten

def get_normal_shanten(counts):
    """一般型：递归求解 8 - 2*面子 - 搭子 - 雀头带来的补正"""
    # 由于需要尝试是否有雀头，我们分为“有雀头”和“无雀头”两种情况去搜索
    
    min_shanten = 8 # 初始最大值
    
    # 遍历所有可能的雀头
    for i in range(34):
        if counts[i] >= 2:
            counts[i] -= 2
            # 此时标准是4个面子，已有雀头
            val = run_normal_search(counts, 0, 0)
            counts[i] += 2
            min_shanten = min(min_shanten, val - 1) # 有雀头，向听数 -1
            
    # 不带雀头的情况（4面子+1雀头，但暂时还没找到雀头，可能单骑）
    val_no_head = run_normal_search(counts, 0, 0)
    min_shanten = min(min_shanten, val_no_head)
    
    return min_shanten

def run_normal_search(counts, groups, partials):
    """
    深度优先搜索提取面子和搭子
    counts: 剩余牌
    groups: 已提取的面子数 (顺子/刻子)
    partials: 已提取的搭子数 (两面/嵌张/边张/对子)
    """
    # 剪枝：如果已经提取足够多的组合，直接计算
    # 在标准向听计算中，我们最多需要 4 个 block (面子+搭子)
    if groups + partials > 4: 
        # 即使超过了，也按4个算，因为多出来的无用
        # 公式：8 - 2*groups - partials
        # 这里的partials 实际上包含多余的，但我们在下面遍历时控制
        return 8 - 2 * groups - partials

    # 寻找第一个非0的牌
    first = -1
    for i in range(34):
        if counts[i] > 0:
            first = i
            break
            
    if first == -1:
        # 牌已取完
        # 限制有效组数不超过4
        current_groups = groups
        current_partials = partials
        if current_groups + current_partials > 4:
            current_partials = 4 - current_groups
        return 8 - 2 * current_groups - current_partials

    # 1. 尝试提取刻子 (e.g. 111)
    best_res = 8 
    if counts[first] >= 3:
        counts[first] -= 3
        best_res = min(best_res, run_normal_search(counts, groups + 1, partials))
        counts[first] += 3
        
    # 2. 尝试提取顺子 (只针对数牌 0-26)
    # 字牌(27+)不能组成顺子
    # 且 8m, 9m, 8p... 不能作为顺子开头
    if first < 27 and (first % 9) < 7:
        if counts[first+1] > 0 and counts[first+2] > 0:
            counts[first] -= 1
            counts[first+1] -= 1
            counts[first+2] -= 1
            best_res = min(best_res, run_normal_search(counts, groups + 1, partials))
            counts[first] += 1
            counts[first+1] += 1
            counts[first+2] += 1
            
    # 3. 尝试提取搭子 (作为 partials)
    # 3.1 对子搭子
    if counts[first] >= 2:
        counts[first] -= 2
        best_res = min(best_res, run_normal_search(counts, groups, partials + 1))
        counts[first] += 2
        
    # 3.2 顺子搭子 (两面/嵌张/边张)
    if first < 27:
        # 这里的逻辑稍微复杂，为了简化向听计算，只要是邻近两张都算搭子
        # 检查 first+1
        if (first % 9) < 8 and counts[first+1] > 0:
            counts[first] -= 1
            counts[first+1] -= 1
            best_res = min(best_res, run_normal_search(counts, groups, partials + 1))
            counts[first] += 1
            counts[first+1] += 1
        # 检查 first+2 (嵌张)
        if (first % 9) < 7 and counts[first+2] > 0:
            counts[first] -= 1
            counts[first+2] -= 1
            best_res = min(best_res, run_normal_search(counts, groups, partials + 1))
            counts[first] += 1
            counts[first+2] += 1

    # 4. 如果以上都组不了，只能将这张牌作为孤张处理，跳过
    # 注意：为了遍历完全，必须有这个分支，否则会卡死在无法成组的牌上
    counts[first] -= 1
    best_res = min(best_res, run_normal_search(counts, groups, partials))
    counts[first] += 1
    
    return best_res

# --- 3. 牌理分析器 ---

def analyze_efficiency(hand_str_list):
    """
    分析手牌，返回何切列表
    假设当前手牌是 14 张（或者 13 张 + 摸到 1 张）
    """
    counts = tiles_to_counts(hand_str_list)
    current_shanten = calculate_shanten(counts)
    
    print(f"当前手牌向听数: {current_shanten} 向听")
    if current_shanten == -1:
        print("也就是：已和牌")
        return

    # 获取所有独特的每一张手牌（去重，避免重复计算打1m和打另一张1m）
    unique_tiles_in_hand = sorted(list(set([t for t in range(34) if counts[t] > 0])))
    
    results = []

    for discard_tile in unique_tiles_in_hand:
        # 模拟打出这张牌
        counts[discard_tile] -= 1
        
        # 寻找“有效进张”
        # 有效进张的定义：摸到这张牌后，向听数 < current_shanten
        effective_tiles = []
        total_remaining = 0
        
        for draw_tile in range(34):
            if counts[draw_tile] == 4: # 牌山里没了（假设不考虑副露和他家舍牌）
                continue
                
            counts[draw_tile] += 1
            new_shanten = calculate_shanten(counts)
            counts[draw_tile] -= 1
            
            if new_shanten < current_shanten:
                # 这是一个有效进张
                # 剩余张数 = 4 - 手里已有的张数
                left = 4 - counts[draw_tile]
                effective_tiles.append(draw_tile)
                total_remaining += left
        
        if len(effective_tiles) > 0:
            results.append({
                'discard': TILES[discard_tile],
                'wait_tiles': [TILES[t] for t in effective_tiles],
                'count': total_remaining
            })
            
        # 恢复手牌
        counts[discard_tile] += 1

    # 排序：按进张总数（count）降序排列
    results.sort(key=lambda x: x['count'], reverse=True)
    
    return results

# --- 4. 主程序入口 ---

if __name__ == "__main__":
    # 示例手牌：比较常见的何切问题
    # 这是一个一向听的手牌
    # 假设输入格式为列表
    my_hand = [
        '2m', '3m', '3m', '4m', '4m',  # 万子
        '1p', '2p', '9p',             # 筒子
        '3s', '6s', '8s','8s',           # 索子
        '发', '中'               # 字牌
    ]
    
    print(f"当前手牌: {' '.join(my_hand)}")
    print("-" * 30)
    
    analysis = analyze_efficiency(my_hand)
    
    if analysis:
        print(f"{'打':<6} {'有效进张总数':<10} {'进张牌型'}")
        print("-" * 40)
        for res in analysis:
            waits_str = " ".join(res['wait_tiles'])
            print(f"{res['discard']:<6} {res['count']:<14} {waits_str}")
    else:
        print("无法计算或已经是和牌/完全没听。")