import bpy
import bmesh

# 1. 确保 Cube 是当前选中的活动物体
cube = bpy.context.scene.objects["Cube"]
bpy.context.view_layer.objects.active = cube

# 2. 切换到编辑模式
bpy.ops.object.mode_set(mode='EDIT')

# 3. 此时再获取 bmesh 就不会报错了
bm = bmesh.from_edit_mesh(cube.data)

# 4. 注意：在编辑模式下做完修改后，需要更新网格
bmesh.update_edit_mesh(cube.data)
