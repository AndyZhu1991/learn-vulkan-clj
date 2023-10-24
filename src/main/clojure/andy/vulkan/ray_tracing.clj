;; Based on the 01_InitRaytracing.cpp example from VkRayTutorials
;; Copyright (c) 2018 Adrian Medina

(ns andy.vulkan.ray-tracing
  (:gen-class)
  (:require
   [vk.ray.tutorials.util :as util])
  (:import
   (java.nio ByteBuffer FloatBuffer IntBuffer LongBuffer)
   (org.lwjgl PointerBuffer)
   (org.lwjgl.system MemoryUtil)
   (org.lwjgl.glfw GLFW GLFWVulkan)
   (org.lwjgl.glfw GLFWKeyCallback GLFWKeyCallbackI
                   GLFWWindowSizeCallback GLFWWindowSizeCallbackI)
   (org.lwjgl.vulkan EXTDebugReport KHRSwapchain KHRSurface VK11)
   (org.lwjgl.vulkan VkApplicationInfo
                     VkAttachmentDescription
                     VkAttachmentReference
                     VkBufferCreateInfo
                     VkClearValue
                     VkCommandBuffer
                     VkCommandBufferAllocateInfo
                     VkCommandBufferBeginInfo
                     VkCommandPoolCreateInfo
                     VkDebugReportCallbackCreateInfoEXT
                     VkDebugReportCallbackEXT
                     VkDebugReportCallbackEXTI
                     VkDevice
                     VkDeviceCreateInfo
                     VkDeviceQueueCreateInfo
                     VkExtent2D
                     VkFramebufferCreateInfo
                     VkGraphicsPipelineCreateInfo
                     VkImageMemoryBarrier
                     VkImageViewCreateInfo
                     VkInstance
                     VkInstanceCreateInfo
                     VkMemoryAllocateInfo
                     VkMemoryRequirements
                     VkPhysicalDevice
                     VkPhysicalDeviceMemoryProperties
                     VkPipelineColorBlendAttachmentState
                     VkPipelineColorBlendStateCreateInfo
                     VkPipelineDepthStencilStateCreateInfo
                     VkPipelineDynamicStateCreateInfo
                     VkPipelineInputAssemblyStateCreateInfo
                     VkPipelineLayoutCreateInfo
                     VkPipelineMultisampleStateCreateInfo
                     VkPipelineRasterizationStateCreateInfo
                     VkPipelineShaderStageCreateInfo
                     VkPipelineVertexInputStateCreateInfo
                     VkPipelineViewportStateCreateInfo
                     VkPresentInfoKHR
                     VkQueue
                     VkQueueFamilyProperties
                     VkRect2D
                     VkRenderPassBeginInfo
                     VkRenderPassCreateInfo
                     VkSemaphoreCreateInfo
                     VkShaderModuleCreateInfo
                     VkSubmitInfo
                     VkSubpassDescription
                     VkSurfaceFormatKHR
                     VkSurfaceCapabilitiesKHR
                     VkSwapchainCreateInfoKHR
                     VkVertexInputAttributeDescription
                     VkVertexInputBindingDescription
                     VkViewport)))

(def ^:private validation?
  (Boolean/parseBoolean (System/getProperty "vulkan.validation" "false")))

(def ^:private layers
  (into-array ByteBuffer [(util/utf8 "A validation layer found an error.")]))

(def ^:private ^:const vk-flags-none 0)

(def ^:private ^:const uint64-max -1)

(defonce ^:private resources
  (atom {:must-recreate?              true
         :swapchain                   nil
         :framebuffers                nil
         :width                       800
         :height                      600
         :render-command-buffers      nil
         :setup-command-buffer        nil
         :color-format-and-space      nil
         :command-pool                nil
         :post-present-command-buffer nil
         :queue                       nil
         :render-pass                 nil
         :render-command-pool         nil
         :vertices                    nil
         :pipeline                    nil
         :physical-device             nil}))

(defn- ^VkInstance create-instance
  [^PointerBuffer required-extensions]
  (let [app-info                   (.. (VkApplicationInfo/calloc)
                                       (sType VK11/VK_STRUCTURE_TYPE_APPLICATION_INFO)
                                       (pApplicationName (util/utf8 "GLFW Vulkan Demo"))
                                       (pEngineName (util/utf8 ""))
                                       (apiVersion (VK11/VK_MAKE_VERSION 1 1 85)))
        ptr                        (MemoryUtil/memAllocPointer (inc (.remaining required-extensions)))
        pp-enabled-extension-names (doto ptr
                                     (.put required-extensions))
        vk-ext-debug-report-ext    (util/utf8 EXTDebugReport/VK_EXT_DEBUG_REPORT_EXTENSION_NAME)
        pp-enabled-extension-names (doto pp-enabled-extension-names
                                     (.put vk-ext-debug-report-ext)
                                     (.flip))
        ptr                        (MemoryUtil/memAllocPointer (alength layers))
        _                          (when validation?
                                     (dotimes [i (alength layers)]
                                       (.put ptr (aget layers i))))
        pp-enabled-layer-names     (doto ptr
                                     (.flip))
        p-create-info              (.. (VkInstanceCreateInfo/calloc)
                                       (sType VK11/VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO)
                                       (pNext 0)
                                       (pApplicationInfo app-info)
                                       (ppEnabledExtensionNames pp-enabled-extension-names)
                                       (ppEnabledLayerNames pp-enabled-layer-names))
        p-instance                 (MemoryUtil/memAllocPointer 1)
        err                        (VK11/vkCreateInstance p-create-info nil p-instance)
        instance                   (.get p-instance 0)
        _                          (assert (= err VK11/VK_SUCCESS)
                                           (str "Failed to create VkInstance: " (util/error-msg err)))
        _                          (MemoryUtil/memFree p-instance)
        instance                   (VkInstance. instance p-create-info)
        _                          (.free p-create-info)
        _                          (MemoryUtil/memFree pp-enabled-layer-names)
        _                          (MemoryUtil/memFree pp-enabled-extension-names)
        _                          (MemoryUtil/memFree (.pApplicationName app-info))
        _                          (MemoryUtil/memFree (.pEngineName app-info))
        _                          (.free app-info)]
    instance))

(defn- setup-debugging
  [instance flags callback]
  (let [dbg-create-info (.. (VkDebugReportCallbackCreateInfoEXT/calloc)
                            (sType EXTDebugReport/VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT)
                            (pNext 0)
                            (pfnCallback callback)
                            (pUserData 0)
                            (flags flags))
        p-callback      (MemoryUtil/memAllocLong 1)
        err             (EXTDebugReport/vkCreateDebugReportCallbackEXT instance
                                                                       dbg-create-info
                                                                       nil
                                                                       p-callback)
        callback-handle (.get p-callback 0)
        _               (MemoryUtil/memFree p-callback)
        _               (.free dbg-create-info)]
    (assert (= err VK11/VK_SUCCESS)
            (str "Failed to create VkInstance: " (util/error-msg err)))
    callback-handle))

(defn get-first-physical-device
  [instance]
  (let [p-physical-device-count (MemoryUtil/memAllocInt 1)
        err                     (VK11/vkEnumeratePhysicalDevices instance
                                                                 p-physical-device-count
                                                                 nil)
        _                       (assert (= err VK11/VK_SUCCESS)
                                        (str "Failed to get number of physical devices: "
                                             (util/error-msg err)))
        physical-device-count   (.get p-physical-device-count 0)
        p-physical-devices      (MemoryUtil/memAllocPointer physical-device-count)
        err                     (VK11/vkEnumeratePhysicalDevices instance
                                                                 p-physical-device-count
                                                                 p-physical-devices)
        physical-device         (.get p-physical-devices 0)
        _                       (MemoryUtil/memFree p-physical-device-count)
        _                       (MemoryUtil/memFree p-physical-devices)]
    (assert (= err VK11/VK_SUCCESS)
            (str "Failed to get physical devices: " (util/error-msg err)))
    (VkPhysicalDevice. physical-device instance)))

(defn- create-device-and-get-graphics-queue-family
  [physical-device]
  (let [p-queue-fam-prop-count
        (MemoryUtil/memAllocInt 1)
        _
        (VK11/vkGetPhysicalDeviceQueueFamilyProperties physical-device
                                                       p-queue-fam-prop-count
                                                       nil)
        queue-count
        (.get p-queue-fam-prop-count 0)
        queue-props
        (VkQueueFamilyProperties/calloc queue-count)
        _
        (VK11/vkGetPhysicalDeviceQueueFamilyProperties physical-device
                                                       p-queue-fam-prop-count
                                                       queue-props)
        _
        (MemoryUtil/memFree p-queue-fam-prop-count)
        graphics-queue-family-index
        (loop [i 0]
          (if (< i queue-count)
            (if (not (zero? (bit-and (.queueFlags (.get queue-props i))
                                     VK11/VK_QUEUE_GRAPHICS_BIT)))
              i
              (recur (inc i)))
            i))
        _ (.free queue-props)
        p-queue-priorities
        (doto (MemoryUtil/memAllocFloat 1)
          (.put 0.0)
          (.flip))
        queue-create-info
        (.. (VkDeviceQueueCreateInfo/calloc 1)
            (sType VK11/VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO)
            (queueFamilyIndex graphics-queue-family-index)
            (pQueuePriorities p-queue-priorities))
        
        vk-khr-swapchain-extension
        (util/utf8 KHRSwapchain/VK_KHR_SWAPCHAIN_EXTENSION_NAME)
        extensions
        (doto (MemoryUtil/memAllocPointer 1)
          (.put vk-khr-swapchain-extension)
          (.flip))
        pp-enabled-layer-names
        (MemoryUtil/memAllocPointer (alength layers))
        _
        (when validation?
          (dotimes [i (alength layers)]
            (.put pp-enabled-layer-names (aget layers i))))
        _
        (.flip pp-enabled-layer-names)
        device-create-info
        (.. (VkDeviceCreateInfo/calloc)
            (sType VK11/VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO)
            (pNext 0)
            (pQueueCreateInfos queue-create-info)
            (ppEnabledExtensionNames extensions)
            (ppEnabledLayerNames pp-enabled-layer-names))
        p-device          (MemoryUtil/memAllocPointer 1)
        err
        (VK11/vkCreateDevice physical-device
                             device-create-info
                             nil
                             p-device)
        device
        (.get p-device 0)
        _                 (MemoryUtil/memFree p-device)
        _                 (assert (= err VK11/VK_SUCCESS)
                                  (str "Failed to create device: " (util/error-msg err)))
        memory-properties (VkPhysicalDeviceMemoryProperties/calloc)
        _                 (VK11/vkGetPhysicalDeviceMemoryProperties physical-device
                                                                    memory-properties)]
    (try
      {:device             (VkDevice. device physical-device device-create-info)
       :queue-family-index graphics-queue-family-index
       :memory-properties  memory-properties}
      (finally
        (.free device-create-info)
        (MemoryUtil/memFree pp-enabled-layer-names)
        (MemoryUtil/memFree vk-khr-swapchain-extension)
        (MemoryUtil/memFree extensions)
        (MemoryUtil/memFree p-queue-priorities)))))

(defn- get-color-format-and-space
  [physical-device surface]
  (let [p-queue-family-prop-count (MemoryUtil/memAllocInt 1)
        _
        (VK11/vkGetPhysicalDeviceQueueFamilyProperties physical-device
                                                       p-queue-family-prop-count
                                                       nil)
        queue-count               (.get p-queue-family-prop-count 0)
        queue-props               (VkQueueFamilyProperties/calloc queue-count)
        _
        (VK11/vkGetPhysicalDeviceQueueFamilyProperties physical-device
                                                       p-queue-family-prop-count
                                                       queue-props)
        _                         (MemoryUtil/memFree p-queue-family-prop-count)
        supports-present          (MemoryUtil/memAllocInt queue-count)
        _
        (dotimes [i queue-count]
          (.position supports-present i)
          (let [err (KHRSurface/vkGetPhysicalDeviceSurfaceSupportKHR
                      physical-device i surface supports-present)]
            (util/assert-vk-success
              err
              "Physical device does not support KHR surfaces: ")))
        [graphics-queue-node-index present-queue-node-index]
        (loop [graphics-queue-node-index Integer/MAX_VALUE
               present-queue-node-index  Integer/MAX_VALUE
               i                         0]
          (if (< i queue-count)
            (if (not (zero? (bit-and (.queueFlags (.get queue-props i))
                                     VK11/VK_QUEUE_GRAPHICS_BIT)))
              (cond
                (== graphics-queue-node-index Integer/MAX_VALUE)
                (recur i present-queue-node-index (inc i))
                (== (.get supports-present i) VK11/VK_TRUE)
                [i i]
                :else (recur graphics-queue-node-index
                             present-queue-node-index
                             (inc i)))
              (recur graphics-queue-node-index
                     present-queue-node-index
                     (inc i)))
            [graphics-queue-node-index
             (if (== present-queue-node-index Integer/MAX_VALUE)
               (reduce (fn [present-queue-node-index i]
                         (if (== (.get supports-present i) VK11/VK_TRUE)
                           (reduced i)
                           present-queue-node-index))
                       present-queue-node-index (range queue-count))
               present-queue-node-index)]))
        _                         (.free queue-props)
        _                         (MemoryUtil/memFree supports-present)
        _                         (assert (not (== graphics-queue-node-index Integer/MAX_VALUE))
                                          "No graphics queue found.")
        _                         (assert (not (== present-queue-node-index Integer/MAX_VALUE))
                                          "No presentation queue found.")
        _                         (assert (== graphics-queue-node-index present-queue-node-index)
                                          "Presentation queue != graphics queue")
        p-format-count            (MemoryUtil/memAllocInt 1)
        err                       (KHRSurface/vkGetPhysicalDeviceSurfaceFormatsKHR physical-device
                                                                                   surface
                                                                                   p-format-count
                                                                                   nil)
        format-count              (.get p-format-count 0)
        _                         (assert (= err VK11/VK_SUCCESS)
                                          (str "Failed to query physical device surface formats: "
                                               (util/error-msg err)))
        surface-formats           (VkSurfaceFormatKHR/calloc format-count)
        err                       (KHRSurface/vkGetPhysicalDeviceSurfaceFormatsKHR physical-device
                                                                                   surface
                                                                                   p-format-count
                                                                                   surface-formats)
        _                         (assert (= err VK11/VK_SUCCESS)
                                          (str "Failed to query physical device surface formats: "
                                               (util/error-msg err)))
        color-format              (if (and (== format-count 1)
                                           (== (.format (.get surface-formats 0))
                                               VK11/VK_FORMAT_UNDEFINED))
                                    VK11/VK_FORMAT_B8G8R8A8_UNORM
                                    (.format (.get surface-formats 0)))
        color-space               (.colorSpace (.get surface-formats 0))
        _                         (.free surface-formats)]
    {:color-format color-format
     :color-space  color-space}))

(defn- create-command-pool
  [device queue-node-index]
  (let [command-pool-info (.. (VkCommandPoolCreateInfo/calloc)
                              (sType VK11/VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO)
                              (queueFamilyIndex queue-node-index)
                              (flags VK11/VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT))
        p-cmd-pool        (MemoryUtil/memAllocLong 1)
        err               (VK11/vkCreateCommandPool device command-pool-info nil p-cmd-pool)
        command-pool      (.get p-cmd-pool 0)]
    (.free command-pool-info)
    (MemoryUtil/memFree p-cmd-pool)
    (assert (== err VK11/VK_SUCCESS)
            (str "Failed to create command pool: " (util/error-msg err)))
    command-pool))

(defn- create-device-queue
  [device queue-family-index]
  (let [p-queue (MemoryUtil/memAllocPointer 1)
        _       (VK11/vkGetDeviceQueue device queue-family-index 0 p-queue)
        queue   (.get p-queue 0)]
    (MemoryUtil/memFree p-queue)
    (VkQueue. queue device)))

(defn- create-command-buffer
  [device command-pool]
  (let [cmd-buf-allocate-info (.. (VkCommandBufferAllocateInfo/calloc)
                                  (sType VK11/VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO)
                                  (commandPool command-pool)
                                  (level VK11/VK_COMMAND_BUFFER_LEVEL_PRIMARY)
                                  (commandBufferCount 1))
        p-command-buffer      (MemoryUtil/memAllocPointer 1)
        err                   (VK11/vkAllocateCommandBuffers device
                                                             cmd-buf-allocate-info
                                                             p-command-buffer)
        _                     (.free cmd-buf-allocate-info)
        command-buffer        (.get p-command-buffer 0)
        _                     (MemoryUtil/memFree p-command-buffer)]
    (assert (== err VK11/VK_SUCCESS)
            (str "Failed to allocate command buffer: " (util/error-msg err)))
    (VkCommandBuffer. command-buffer device)))

(defn- image-barrier
  [cmd-buf img aspect-mask old-img-layout src-access new-img-layout dst-access]
  (let [img-mem-barrier (.. (VkImageMemoryBarrier/calloc 1)
                            (sType VK11/VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER)
                            (pNext 0)
                            (oldLayout old-img-layout)
                            (srcAccessMask src-access)
                            (newLayout new-img-layout)
                            (dstAccessMask dst-access)
                            (srcQueueFamilyIndex VK11/VK_QUEUE_FAMILY_IGNORED)
                            (dstQueueFamilyIndex VK11/VK_QUEUE_FAMILY_IGNORED)
                            (image img))
        _               (.. (.subresourceRange img-mem-barrier)
                            (aspectMask aspect-mask)
                            (baseMipLevel 0)
                            (levelCount 1)
                            (layerCount 1))
        src-stage-flags VK11/VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
        dst-stage-flags VK11/VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT]
    (VK11/vkCmdPipelineBarrier cmd-buf src-stage-flags dst-stage-flags
                               vk-flags-none nil nil img-mem-barrier)
    (.free img-mem-barrier)))

(defn- create-swapchain
  [device physical-device surface old-swapchain command-buffer
   new-width new-height color-format color-space]
  (let [surface-caps          (VkSurfaceCapabilitiesKHR/calloc)
        err                   (KHRSurface/vkGetPhysicalDeviceSurfaceCapabilitiesKHR
                                physical-device surface surface-caps)
        _                     (assert (== err VK11/VK_SUCCESS)
                                      (str "Failed to get physical device surface capabilities: "
                                           (util/error-msg err)))
        p-present-mode-count  (MemoryUtil/memAllocInt 1)
        err                   (KHRSurface/vkGetPhysicalDeviceSurfacePresentModesKHR 
                                physical-device surface p-present-mode-count nil)
        present-mode-count    (.get p-present-mode-count 0)
        _                     (assert (== err VK11/VK_SUCCESS)
                                      (str "Failed to get number of physical device surface presentation modes: "
                                           (util/error-msg err)))
        p-present-modes       (MemoryUtil/memAllocInt present-mode-count)
        err                   (KHRSurface/vkGetPhysicalDeviceSurfacePresentModesKHR 
                                physical-device surface p-present-mode-count p-present-modes)
        _                     (MemoryUtil/memFree p-present-mode-count)
        _                     (assert (== err VK11/VK_SUCCESS)
                                      (str "Failed to get physical device surface presentation modes: "
                                           (util/error-msg err)))
        swapchain-present-mode
        (loop [swapchain-present-mode KHRSurface/VK_PRESENT_MODE_FIFO_KHR
               i                      0]
          (if (< i present-mode-count)
            (if (== (.get p-present-modes i)
                    KHRSurface/VK_PRESENT_MODE_MAILBOX_KHR)
              KHRSurface/VK_PRESENT_MODE_MAILBOX_KHR
              (recur (if (and (not (== swapchain-present-mode
                                       KHRSurface/VK_PRESENT_MODE_MAILBOX_KHR))
                              (== (.get p-present-modes i)
                                  KHRSurface/VK_PRESENT_MODE_IMMEDIATE_KHR))
                       KHRSurface/VK_PRESENT_MODE_IMMEDIATE_KHR
                       swapchain-present-mode)
                     (inc i)))
            swapchain-present-mode))
        _                     (MemoryUtil/memFree p-present-modes)
        desired-number-of-swapchain-images
        (as-> (inc (.minImageCount surface-caps)) n
          (if (and (pos? (.maxImageCount surface-caps))
                   (> n (.maxImageCount surface-caps)))
            (.maxImageCount surface-caps)
            n))
        current-extent        (.currentExtent surface-caps)
        current-width         (.width current-extent)
        current-height        (.height current-extent)
        width                 (if (pos? current-width)
                                current-width
                                new-width)
        height                (if (pos? current-height)
                                current-height
                                new-height)
        pre-transform         (if (not (zero? (bit-and (.supportedTransforms surface-caps)
                                                       KHRSurface/VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR)))
                                KHRSurface/VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR
                                (.currentTransform surface-caps))
        _                     (.free surface-caps)
        swapchain-ci          (.. (VkSwapchainCreateInfoKHR/calloc)
                                  (sType KHRSwapchain/VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR)
                                  (pNext 0)
                                  (surface surface)
                                  (minImageCount desired-number-of-swapchain-images)
                                  (imageFormat color-format)
                                  (imageColorSpace color-space)
                                  (imageUsage VK11/VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT)
                                  (preTransform pre-transform)
                                  (imageArrayLayers 1)
                                  (imageSharingMode VK11/VK_SHARING_MODE_EXCLUSIVE)
                                  (pQueueFamilyIndices nil)
                                  (presentMode swapchain-present-mode)
                                  (oldSwapchain old-swapchain)
                                  (clipped true)
                                  (compositeAlpha KHRSurface/VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR))
        _                     (.. (.imageExtent swapchain-ci)
                                  (width width)
                                  (height height))
        p-swapchain           (MemoryUtil/memAllocLong 1)
        err                   (KHRSwapchain/vkCreateSwapchainKHR device
                                                                 swapchain-ci
                                                                 nil
                                                                 p-swapchain)
        _                     (.free swapchain-ci)
        swapchain             (.get p-swapchain 0)
        _                     (MemoryUtil/memFree p-swapchain)
        _                     (assert (== err VK11/VK_SUCCESS)
                                      (str "Failed to create swapchain: " (util/error-msg err)))
        _                     (when-not (== old-swapchain VK11/VK_NULL_HANDLE)
                                (KHRSwapchain/vkDestroySwapchainKHR device old-swapchain nil))
        p-image-count         (MemoryUtil/memAllocInt 1)
        err                   (KHRSwapchain/vkGetSwapchainImagesKHR device
                                                                    swapchain
                                                                    p-image-count
                                                                    nil)
        image-count           (.get p-image-count 0)
        _                     (assert (== err VK11/VK_SUCCESS)
                                      (str "Failed to get number of swapchain images: "
                                           (util/error-msg err)))
        p-swapchain-images    (MemoryUtil/memAllocLong image-count)
        err                   (KHRSwapchain/vkGetSwapchainImagesKHR device
                                                                    swapchain
                                                                    p-image-count
                                                                    p-swapchain-images)
        _                     (assert (== err VK11/VK_SUCCESS)
                                      (str "Failed to get swapchain images: " (util/error-msg err)))
        _                     (MemoryUtil/memFree p-image-count)
        images                (long-array image-count)
        image-views           (long-array image-count)
        p-buffer-view         (MemoryUtil/memAllocLong 1)
        color-attachment-view (.. (VkImageViewCreateInfo/calloc)
                                  (sType VK11/VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO)
                                  (pNext 0)
                                  (format color-format)
                                  (viewType VK11/VK_IMAGE_VIEW_TYPE_2D)
                                  (flags vk-flags-none))
        _                     (.. (.components color-attachment-view)
                                  (r VK11/VK_COMPONENT_SWIZZLE_R)
                                  (g VK11/VK_COMPONENT_SWIZZLE_G)
                                  (b VK11/VK_COMPONENT_SWIZZLE_B)
                                  (a VK11/VK_COMPONENT_SWIZZLE_A))
        _                     (.. (.subresourceRange color-attachment-view)
                                  (aspectMask VK11/VK_IMAGE_ASPECT_COLOR_BIT)
                                  (baseMipLevel 0)
                                  (levelCount 1)
                                  (baseArrayLayer 0)
                                  (layerCount 1))]
    (dotimes [i image-count]
      (aset images i (.get p-swapchain-images i))
      (image-barrier command-buffer
                     (aget images i)
                     VK11/VK_IMAGE_ASPECT_COLOR_BIT
                     VK11/VK_IMAGE_LAYOUT_UNDEFINED
                     0
                     VK11/VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                     VK11/VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)
      (.image color-attachment-view (aget images i))
      (let [err (VK11/vkCreateImageView device
                                        color-attachment-view
                                        nil
                                        p-buffer-view)]
        (aset image-views i (.get p-buffer-view 0))
        (assert (== err VK11/VK_SUCCESS)
                (str "Failed to create image view: " (util/error-msg err)))))
    (.free color-attachment-view)
    (MemoryUtil/memFree p-buffer-view)
    (MemoryUtil/memFree p-swapchain-images)
    {:images           images
     :image-views      image-views
     :swapchain-handle swapchain}))

(defn- create-render-pass
  [device color-format]
  (let [attachments      (.. (VkAttachmentDescription/calloc 1)
                             (format color-format)
                             (samples VK11/VK_SAMPLE_COUNT_1_BIT)
                             (loadOp VK11/VK_ATTACHMENT_LOAD_OP_CLEAR)
                             (storeOp VK11/VK_ATTACHMENT_STORE_OP_STORE)
                             (stencilLoadOp VK11/VK_ATTACHMENT_LOAD_OP_DONT_CARE)
                             (stencilStoreOp VK11/VK_ATTACHMENT_STORE_OP_DONT_CARE)
                             (initialLayout VK11/VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL)
                             (finalLayout VK11/VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL))
        color-ref        (.. (VkAttachmentReference/calloc 1)
                             (attachment 0)
                             (layout VK11/VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL))
        subpass          (.. (VkSubpassDescription/calloc 1)
                             (pipelineBindPoint VK11/VK_PIPELINE_BIND_POINT_GRAPHICS)
                             (flags vk-flags-none)
                             (pInputAttachments nil)
                             (colorAttachmentCount (.remaining color-ref))
                             (pColorAttachments color-ref)
                             (pResolveAttachments nil)
                             (pDepthStencilAttachment nil)
                             (pPreserveAttachments nil))
        render-pass-info (.. (VkRenderPassCreateInfo/calloc)
                             (sType VK11/VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO)
                             (pNext 0)
                             (pAttachments attachments)
                             (pSubpasses subpass)
                             (pDependencies nil))
        p-render-pass    (MemoryUtil/memAllocLong 1)
        err              (VK11/vkCreateRenderPass device render-pass-info nil p-render-pass)
        render-pass      (.get p-render-pass 0)]
    (MemoryUtil/memFree p-render-pass)
    (.free render-pass-info)
    (.free color-ref)
    (.free subpass)
    (.free attachments)
    (assert (== err VK11/VK_SUCCESS)
            (str "Failed to create clear render pass: " (util/error-msg err)))
    render-pass))

(defn- create-framebuffers
  [device swapchain render-pass width height]
  (let [attachments   (MemoryUtil/memAllocLong 1)
        fci           (.. (VkFramebufferCreateInfo/calloc)
                          (sType VK11/VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO)
                          (pAttachments attachments)
                          (flags vk-flags-none)
                          (height height)
                          (width width)
                          (layers 1)
                          (pNext 0)
                          (renderPass render-pass))
        framebuffers  (long-array (alength (:images swapchain)))
        p-framebuffer (MemoryUtil/memAllocLong 1)]
    (dotimes [i (alength (:images swapchain))]
      (.put attachments 0 (aget (:image-views swapchain) i))
      (let [err         (VK11/vkCreateFramebuffer device fci nil p-framebuffer)
            framebuffer (.get p-framebuffer 0)]
        (assert (== err VK11/VK_SUCCESS)
                (str "Failed to create framebuffer: " (util/error-msg err)))
        (aset framebuffers i framebuffer)))
    (MemoryUtil/memFree attachments)
    (MemoryUtil/memFree p-framebuffer)
    (.free fci)
    framebuffers))

(defn- submit-command-buffer
  [queue command-buffer]
  (when (and command-buffer (.address command-buffer))
    (let [submit-info       (.. (VkSubmitInfo/calloc)
                                (sType VK11/VK_STRUCTURE_TYPE_SUBMIT_INFO))
          p-command-buffers (doto (MemoryUtil/memAllocPointer 1)
                              (.put command-buffer)
                              (.flip))
          _                 (.pCommandBuffers submit-info p-command-buffers)
          err               (VK11/vkQueueSubmit queue submit-info VK11/VK_NULL_HANDLE)]
      (MemoryUtil/memFree p-command-buffers)
      (.free submit-info)
      (util/assert-vk-success err "Failed to submit command buffer"))))

(defn- resize-buffer
  [buffer new-capacity]
  (let [new-buffer (org.lwjgl.BufferUtils/createByteBuffer new-capacity)]
    (.flip buffer)
    (.put new-buffer buffer)
    new-buffer))

(defn- ^ByteBuffer io-resource-to-byte-buffer
  [resource buffer-size]
  (let [url  (clojure.java.io/resource resource)
        file (clojure.java.io/as-file (.getFile url))]
    (if (.isFile file)
      (with-open [fis (java.io.FileInputStream. file)
                  fc  (.getChannel fis)]
        (.map fc java.nio.channels.FileChannel$MapMode/READ_ONLY 0 (.size fc)))
      (with-open [source (.openStream url)]
        (when (nil? source)
          (throw (java.io.FileNotFoundException. resource)))
        (with-open [rbc (java.nio.channels.Channels/newChannel source)]
          (loop [buffer (org.lwjgl.BufferUtils/createByteBuffer buffer-size)]
            (let [bytes (.read rbc buffer)]
              (if-not (== bytes -1)
                (do
                  (.flip buffer)
                  buffer)
                (if (zero? (.remaining buffer))
                  (recur (resize-buffer buffer (* (.capacity buffer) 2)))
                  (recur buffer))))))))))

(defn- load-shader
  ([device resource]
   (let [shader-code        (io-resource-to-byte-buffer resource 1024)
         module-create-info (.. (VkShaderModuleCreateInfo/calloc)
                                (sType VK11/VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO)
                                (pNext 0)
                                (pCode shader-code)
                                (flags 0))
         p-shader-module    (MemoryUtil/memAllocLong 1)
         err                (VK11/vkCreateShaderModule device
                                                       module-create-info
                                                       nil
                                                       p-shader-module)
         shader-module      (.get p-shader-module 0)
         _                  (MemoryUtil/memFree p-shader-module)]
     (util/assert-vk-success err "Failed to create shader module")
     shader-module))
  ([device resource stage]
   (.. (VkPipelineShaderStageCreateInfo/calloc)
       (sType VK11/VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO)
       (stage stage)
       (module (load-shader device resource))
       (pName (util/utf8 "main")))))

(defn- get-memory-type
  [device-memory-props type-bits props type-index]
  (loop [bits type-bits
         i    0]
    (if (< i 32)
      (if (== (bit-and bits 1) 1)
        (if (== (bit-and (.propertyFlags (.memoryTypes device-memory-props i))
                         props)
                props)
          (do
            (.put type-index 0 i)
            true)
          (recur (bit-shift-right bits 1) (inc i)))
        (recur (bit-shift-right bits 1) (inc i)))
      false)))

(defn- create-vertices
  [device-memory-props device]
  (let [vertex-buffer     (MemoryUtil/memAlloc (* 3 2 4))
        fb                (doto (.asFloatBuffer vertex-buffer)
                            (.put -0.5)
                            (.put -0.5)
                            (.put 0.5)
                            (.put -0.5)
                            (.put 0.0)
                            (.put 0.5))
        mem-alloc         (.. (VkMemoryAllocateInfo/calloc)
                              (sType VK11/VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO)
                              (pNext 0)
                              (allocationSize 0)
                              (memoryTypeIndex 0))
        mem-reqs          (VkMemoryRequirements/calloc)
        buf-info          (.. (VkBufferCreateInfo/calloc)
                              (sType VK11/VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO)
                              (pNext 0)
                              (size (.remaining vertex-buffer))
                              (usage VK11/VK_BUFFER_USAGE_VERTEX_BUFFER_BIT)
                              (flags 0))
        p-buffer          (MemoryUtil/memAllocLong 1)
        err               (VK11/vkCreateBuffer device buf-info nil p-buffer)
        vertices-buf      (.get p-buffer 0)
        _                 (MemoryUtil/memFree p-buffer)
        _                 (.free buf-info)
        _                 (util/assert-vk-success err "Failed to create vertex buffer")
        _                 (VK11/vkGetBufferMemoryRequirements device vertices-buf mem-reqs)
        _                 (.allocationSize mem-alloc (.size mem-reqs))
        memory-type-index (MemoryUtil/memAllocInt 1)
        _                 (get-memory-type device-memory-props
                                           (.memoryTypeBits mem-reqs)
                                           VK11/VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
                                           memory-type-index)
        _                 (.memoryTypeIndex mem-alloc (.get memory-type-index 0))
        _                 (MemoryUtil/memFree memory-type-index)
        _                 (.free mem-reqs)
        p-memory          (MemoryUtil/memAllocLong 1)
        err               (VK11/vkAllocateMemory device mem-alloc nil p-memory)
        vertices-mem      (.get p-memory 0)
        _                 (MemoryUtil/memFree p-memory)
        _                 (util/assert-vk-success err "Failed to allocate vertex memory")
        p-data            (MemoryUtil/memAllocPointer 1)
        err               (VK11/vkMapMemory device vertices-mem 0 (.allocationSize mem-alloc)
                                            0 p-data)
        _                 (.free mem-alloc)
        data              (.get p-data 0)
        _                 (MemoryUtil/memFree p-data)
        _                 (util/assert-vk-success err "Failed to map vertex memory")
        _                 (MemoryUtil/memCopy (MemoryUtil/memAddress vertex-buffer)
                                              data
                                              (.remaining vertex-buffer))
        _                 (MemoryUtil/memFree vertex-buffer)
        _                 (VK11/vkUnmapMemory device vertices-mem)
        err               (VK11/vkBindBufferMemory device vertices-buf vertices-mem 0)
        _                 (util/assert-vk-success err "Failed to bind memory to vertex buffer")
        binding-desc      (.. (VkVertexInputBindingDescription/calloc 1)
                              (binding 0)
                              (stride (* 2 4))
                              (inputRate VK11/VK_VERTEX_INPUT_RATE_VERTEX))
        attr-desc         (.. (VkVertexInputAttributeDescription/calloc 1)
                              (binding 0)
                              (location 0)
                              (format VK11/VK_FORMAT_R32G32_SFLOAT)
                              (offset 0))
        vi                (.. (VkPipelineVertexInputStateCreateInfo/calloc)
                              (sType VK11/VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO)
                              (pNext 0)
                              (pVertexBindingDescriptions binding-desc)
                              (pVertexAttributeDescriptions attr-desc))]
    {:create-info   vi
     :vertex-buffer vertices-buf}))

(defn- create-pipeline
  [device render-pass vi]
  (let [input-assembly-state
        (.. (VkPipelineInputAssemblyStateCreateInfo/calloc)
            (sType VK11/VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO)
            (topology VK11/VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST))
        rasterization-state
        (.. (VkPipelineRasterizationStateCreateInfo/calloc)
            (sType VK11/VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO)
            (polygonMode VK11/VK_POLYGON_MODE_FILL)
            (cullMode VK11/VK_CULL_MODE_NONE)
            (frontFace VK11/VK_FRONT_FACE_COUNTER_CLOCKWISE)
            (depthClampEnable false)
            (rasterizerDiscardEnable false)
            (depthBiasEnable false))
        color-write-mask
        (.. (VkPipelineColorBlendAttachmentState/calloc 1)
            (blendEnable false)
            (colorWriteMask 0xf))
        color-blend-state
        (.. (VkPipelineColorBlendStateCreateInfo/calloc)
            (sType VK11/VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO)
            (pAttachments color-write-mask))
        viewport-state
        (.. (VkPipelineViewportStateCreateInfo/calloc)
            (sType VK11/VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO)
            (viewportCount 1)
            (scissorCount 1))
        p-dynamic-states
        (doto (MemoryUtil/memAllocInt 2)
          (.put VK11/VK_DYNAMIC_STATE_VIEWPORT)
          (.put VK11/VK_DYNAMIC_STATE_SCISSOR)
          (.flip))
        dynamic-state                 (.. (VkPipelineDynamicStateCreateInfo/calloc)
                                          (sType VK11/VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO)
                                          (pDynamicStates p-dynamic-states))
        depth-stencil-state           (.. (VkPipelineDepthStencilStateCreateInfo/calloc)
                                          (sType VK11/VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO)
                                          (depthTestEnable false)
                                          (depthWriteEnable false)
                                          (depthCompareOp VK11/VK_COMPARE_OP_ALWAYS)
                                          (depthBoundsTestEnable false)
                                          (stencilTestEnable false))
        _                             (.. (.back depth-stencil-state)
                                          (failOp VK11/VK_STENCIL_OP_KEEP)
                                          (passOp VK11/VK_STENCIL_OP_KEEP)
                                          (compareOp VK11/VK_COMPARE_OP_ALWAYS))
        _                             (.front depth-stencil-state (.back depth-stencil-state))
        multisample-state             (.. (VkPipelineMultisampleStateCreateInfo/calloc)
                                          (sType VK11/VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO)
                                          (pSampleMask nil)
                                          (rasterizationSamples VK11/VK_SAMPLE_COUNT_1_BIT))
        shader-stages                 (VkPipelineShaderStageCreateInfo/calloc 2)
        vertex-shader (load-shader device
                                   "vk/ray/tutorials/shaders/triangle.vert.spv"
                                   VK11/VK_SHADER_STAGE_VERTEX_BIT)
        _                             (.set (.get shader-stages 0) vertex-shader)
        fragment-shader (load-shader device
                                     "vk/ray/tutorials/shaders/triangle.frag.spv"
                                     VK11/VK_SHADER_STAGE_FRAGMENT_BIT)
        _                             (.set (.get shader-stages 1) fragment-shader)
        p-pipeline-layout-create-info (.. (VkPipelineLayoutCreateInfo/calloc)
                                          (sType VK11/VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO)
                                          (pNext 0)
                                          (pSetLayouts nil))
        p-pipeline-layout             (MemoryUtil/memAllocLong 1)
        err                           (VK11/vkCreatePipelineLayout device
                                                                   p-pipeline-layout-create-info
                                                                   nil
                                                                   p-pipeline-layout)
        layout                        (.get p-pipeline-layout 0)
        _                             (MemoryUtil/memFree p-pipeline-layout)
        _                             (.free p-pipeline-layout-create-info)
        _                             (util/assert-vk-success err "Failed to create pipeline layout")
        pipeline-create-info          (.. (VkGraphicsPipelineCreateInfo/calloc 1)
                                          (sType VK11/VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO)
                                          (layout layout)
                                          (renderPass render-pass)
                                          (pVertexInputState vi)
                                          (pInputAssemblyState input-assembly-state)
                                          (pRasterizationState rasterization-state)
                                          (pColorBlendState color-blend-state)
                                          (pMultisampleState multisample-state)
                                          (pViewportState viewport-state)
                                          (pDepthStencilState depth-stencil-state)
                                          (pStages shader-stages)
                                          (pDynamicState dynamic-state))
        p-pipelines                   (MemoryUtil/memAllocLong 1)
        err                           (VK11/vkCreateGraphicsPipelines device
                                                                      VK11/VK_NULL_HANDLE
                                                                      pipeline-create-info
                                                                      nil
                                                                      p-pipelines)
        pipeline                      (.get p-pipelines 0)]
    (.free shader-stages)
    (.free multisample-state)
    (.free depth-stencil-state)
    (.free dynamic-state)
    (MemoryUtil/memFree p-dynamic-states)
    (.free viewport-state)
    (.free color-blend-state)
    (.free color-write-mask)
    (.free rasterization-state)
    (.free input-assembly-state)
    (util/assert-vk-success err "Failed to create pipeline")
    pipeline))

(defn- create-pre-present-barrier
  [present-image]
  (let [image-memory-barrier (.. (VkImageMemoryBarrier/calloc 1)
                                 (sType VK11/VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER)
                                 (pNext 0)
                                 (srcAccessMask VK11/VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)
                                 (dstAccessMask 0)
                                 (oldLayout VK11/VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL)
                                 (newLayout KHRSwapchain/VK_IMAGE_LAYOUT_PRESENT_SRC_KHR)
                                 (srcQueueFamilyIndex VK11/VK_QUEUE_FAMILY_IGNORED)
                                 (dstQueueFamilyIndex VK11/VK_QUEUE_FAMILY_IGNORED))]
    (.. (.subresourceRange image-memory-barrier)
        (aspectMask VK11/VK_IMAGE_ASPECT_COLOR_BIT)
        (baseMipLevel 0)
        (levelCount 1)
        (baseArrayLayer 0)
        (layerCount 1))
    (.image image-memory-barrier present-image)
    image-memory-barrier))

(defn- create-render-command-buffers
  [device command-pool framebuffers render-pass width height pipeline vertices swapchain]
  (let [cmd-buf-allocate-info  (.. (VkCommandBufferAllocateInfo/calloc)
                                   (sType VK11/VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO)
                                   (commandPool command-pool)
                                   (level VK11/VK_COMMAND_BUFFER_LEVEL_PRIMARY)
                                   (commandBufferCount (alength framebuffers)))
        p-command-buffer       (MemoryUtil/memAllocPointer (alength framebuffers))
        err                    (VK11/vkAllocateCommandBuffers device
                                                              cmd-buf-allocate-info
                                                              p-command-buffer)
        _                      (util/assert-vk-success err "Failed to allocate render command buffer")
        render-command-buffers (make-array VkCommandBuffer
                                           (alength framebuffers))
        _                      (dotimes [i (alength framebuffers)]
                                 (aset render-command-buffers i (VkCommandBuffer. (.get p-command-buffer i)
                                                                                  device)))
        _                      (MemoryUtil/memFree p-command-buffer)
        _                      (.free cmd-buf-allocate-info)
        cmd-buf-info           (.. (VkCommandBufferBeginInfo/calloc)
                                   (sType VK11/VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO)
                                   (pNext 0))
        clear-values           (VkClearValue/calloc 1)
        _                      (doto (.color clear-values)
                                 (.float32 0 (/ 100.0 255.0))
                                 (.float32 1 (/ 149.0 255.0))
                                 (.float32 2 (/ 237.0 255.0))
                                 (.float32 3 1.0))
        render-pass-begin-info (.. (VkRenderPassBeginInfo/calloc)
                                   (sType VK11/VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO)
                                   (pNext 0)
                                   (renderPass render-pass)
                                   (pClearValues clear-values))
        render-area            (.renderArea render-pass-begin-info)
        _                      (doto (.offset render-area) (.set 0 0))
        _                      (doto (.extent render-area) (.set width height))]
    (dotimes [i (alength render-command-buffers)]
      (.framebuffer render-pass-begin-info (aget framebuffers i))
      (let [render-command-buffer (aget render-command-buffers i)
            err                   (VK11/vkBeginCommandBuffer render-command-buffer cmd-buf-info)
            _                     (util/assert-vk-success err "Failed to begin render command buffer")
            _                     (VK11/vkCmdBeginRenderPass render-command-buffer
                                                             render-pass-begin-info
                                                             VK11/VK_SUBPASS_CONTENTS_INLINE)
            viewport              (.. (VkViewport/calloc 1)
                                      (height height)
                                      (width width)
                                      (minDepth 0.0)
                                      (maxDepth 1.0))
            _                     (VK11/vkCmdSetViewport render-command-buffer 0 viewport)
            _                     (.free viewport)
            scissor               (VkRect2D/calloc 1)
            _                     (doto (.extent scissor) (.set width height))
            _                     (doto (.offset scissor) (.set 0 0))
            _                     (VK11/vkCmdSetScissor render-command-buffer 0 scissor)
            _                     (.free scissor)
            _                     (VK11/vkCmdBindPipeline render-command-buffer
                                                          VK11/VK_PIPELINE_BIND_POINT_GRAPHICS
                                                          pipeline)
            offsets               (doto (MemoryUtil/memAllocLong 1)
                                    (.put 0 0))
            p-buffers             (doto (MemoryUtil/memAllocLong 1)
                                    (.put 0 vertices))
            _                     (VK11/vkCmdBindVertexBuffers render-command-buffer
                                                               0
                                                               p-buffers
                                                               offsets)
            _                     (MemoryUtil/memFree p-buffers)
            _                     (MemoryUtil/memFree offsets)
            _                     (do
                                    (VK11/vkCmdDraw render-command-buffer 3 1 0 0)
                                    (VK11/vkCmdEndRenderPass render-command-buffer))
            swapchain             (aget (:images swapchain) i)
            pre-present-barrier   (create-pre-present-barrier swapchain)
            _                     (VK11/vkCmdPipelineBarrier render-command-buffer
                                                             VK11/VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                                                             VK11/VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
                                                             vk-flags-none
                                                             nil
                                                             nil
                                                             pre-present-barrier)
            _                     (.free pre-present-barrier)
            err                   (VK11/vkEndCommandBuffer render-command-buffer)]
        (util/assert-vk-success err "Failed to begin render command buffer")))
    (.free render-pass-begin-info)
    (.free clear-values)
    (.free cmd-buf-info)
    render-command-buffers))

(defn create-post-present-barrier
  [present-image]
  (let [image-memory-barrier (.. (VkImageMemoryBarrier/calloc 1)
                                 (sType VK11/VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER)
                                 (pNext 0)
                                 (srcAccessMask 0)
                                 (dstAccessMask VK11/VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)
                                 (oldLayout KHRSwapchain/VK_IMAGE_LAYOUT_PRESENT_SRC_KHR)
                                 (newLayout VK11/VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL)
                                 (srcQueueFamilyIndex VK11/VK_QUEUE_FAMILY_IGNORED)
                                 (dstQueueFamilyIndex VK11/VK_QUEUE_FAMILY_IGNORED))]
    (.. (.subresourceRange image-memory-barrier)
        (aspectMask VK11/VK_IMAGE_ASPECT_COLOR_BIT)
        (baseMipLevel 0)
        (levelCount 1)
        (baseArrayLayer 0)
        (layerCount 1))
    (.image image-memory-barrier present-image)
    image-memory-barrier))

(defn submit-post-present-barrier
  [image command-buffer queue]
  (let [cmd-buf-info         (.. (VkCommandBufferBeginInfo/calloc)
                                 (sType VK11/VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO)
                                 (pNext 0))
        err                  (VK11/vkBeginCommandBuffer command-buffer cmd-buf-info)
        _                    (.free cmd-buf-info)
        _                    (util/assert-vk-success err "Failed to begin command buffer")
        post-present-barrier (create-post-present-barrier image)
        _                    (VK11/vkCmdPipelineBarrier command-buffer
                                                        VK11/VK_PIPELINE_STAGE_ALL_COMMANDS_BIT
                                                        VK11/VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
                                                        vk-flags-none
                                                        nil
                                                        nil
                                                        post-present-barrier)
        _                    (.free post-present-barrier)
        err                  (VK11/vkEndCommandBuffer command-buffer)]
    (util/assert-vk-success err "Failed to wait for idle queue")
    (submit-command-buffer queue command-buffer)))

(defn- recreate-swapchain
  ([]
   (reset! resources (recreate-swapchain @resources)))
  ([{:keys [swapchain setup-command-buffer physical-device width height
            color-format-and-space device surface queue framebuffers pipeline
            render-pass render-command-buffers render-command-pool vertices]
     :as resources}]
   (let [cmd-buf-info                       (.. (VkCommandBufferBeginInfo/calloc)
                                                (sType VK11/VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO)
                                                (pNext 0))
         err                                (VK11/vkBeginCommandBuffer setup-command-buffer cmd-buf-info)
         _                                  (.free cmd-buf-info)
         _                                  (util/assert-vk-success err "Failed to begin setup command buffer")
         old-chain                          (if swapchain
                                              (:swapchain-handle swapchain)
                                              VK11/VK_NULL_HANDLE)
         {:keys [color-format color-space]} color-format-and-space
         swapchain                          (create-swapchain device physical-device surface old-chain
                                                              setup-command-buffer width height color-format
                                                              color-space)
         err                                (VK11/vkEndCommandBuffer setup-command-buffer)
         _                                  (util/assert-vk-success err "Failed to end setup command buffer")]
     (submit-command-buffer queue setup-command-buffer)
     (VK11/vkQueueWaitIdle queue)
     (when framebuffers
       (dotimes [i (alength framebuffers)]
         (VK11/vkDestroyFramebuffer device (aget framebuffers i) nil)))
     (when render-command-buffers
       (VK11/vkResetCommandPool device render-command-pool vk-flags-none))
     (let [framebuffers           (create-framebuffers device swapchain render-pass
                                                       width height)
           render-command-buffers (create-render-command-buffers device render-command-pool framebuffers render-pass width height pipeline (:vertex-buffer vertices) swapchain)]
       (assoc resources
              :swapchain swapchain
              :framebuffers framebuffers
              :render-command-buffers render-command-buffers
              :must-recreate? false)))))

(defn -main
  [& args]
  (assert (GLFW/glfwInit) "Failed to initialize GLFW")
  (assert (GLFWVulkan/glfwVulkanSupported) "GLFW failed to load Vulkan")
  (let [req-ext                     (GLFWVulkan/glfwGetRequiredInstanceExtensions)
        _                           (assert req-ext "Failed to find list of required Vulkan extensions")
        instance                    (create-instance req-ext)
        debug-callback-f            (reify VkDebugReportCallbackEXTI
                                      (invoke [this flags type object loc message-code
                                               p-layer-prefix p-message p-user-data]
                                        (binding [*out* *err*]
                                          (println (str "ERROR OCCURED: "
                                                        (VkDebugReportCallbackEXT/getString p-message))))
                                        0))
        debug-callback              (VkDebugReportCallbackEXT/create debug-callback-f)
        debug-flags                 (bit-or EXTDebugReport/VK_DEBUG_REPORT_ERROR_BIT_EXT
                                            EXTDebugReport/VK_DEBUG_REPORT_WARNING_BIT_EXT)
        debug-callback-handle       (setup-debugging instance
                                                     debug-flags
                                                     debug-callback)
        physical-device             (get-first-physical-device instance)
        queue-family                (create-device-and-get-graphics-queue-family physical-device)
        device                      (:device queue-family)
        queue-family-index          (:queue-family-index queue-family)
        memory-props                (:memory-properties queue-family)
        _                           (do
                                      (GLFW/glfwDefaultWindowHints)
                                      (GLFW/glfwWindowHint GLFW/GLFW_CLIENT_API GLFW/GLFW_NO_API)
                                      (GLFW/glfwWindowHint GLFW/GLFW_VISIBLE GLFW/GLFW_FALSE))
        window                      (GLFW/glfwCreateWindow 800 600 (util/utf8 "GLFW Vulkan Demo") 0 0)
        key-callback                (reify GLFWKeyCallbackI
                                      (invoke [this window key scancode action mods]
                                        (when (= action GLFW/GLFW_RELEASE)
                                          (when (= key GLFW/GLFW_KEY_ESCAPE)
                                            (GLFW/glfwSetWindowShouldClose window true)))))
        _                           (GLFW/glfwSetKeyCallback window (GLFWKeyCallback/create key-callback))
        p-surface                   (MemoryUtil/memAllocLong 1)
        err                         (GLFWVulkan/glfwCreateWindowSurface instance window nil p-surface)
        surface                     (.get p-surface 0)
        _                           (util/assert-vk-success err "Failed to create surface")
        color-format-and-space      (get-color-format-and-space physical-device
                                                                surface)
        command-pool                (create-command-pool device queue-family-index)
        setup-command-buffer        (create-command-buffer device command-pool)
        post-present-command-buffer (create-command-buffer device command-pool)
        queue                       (create-device-queue device queue-family-index)
        render-pass                 (create-render-pass device (:color-format color-format-and-space))
        render-command-pool         (create-command-pool device queue-family-index)
        vertices                    (create-vertices memory-props device)
        pipeline                    (create-pipeline device render-pass (:create-info vertices))
        window-size-callback        (reify GLFWWindowSizeCallbackI
                                      (invoke [this window width height]
                                        (when (and (pos? width) (pos? height))
                                          (swap! resources assoc
                                                 :width width
                                                 :height height
                                                 :must-recreate? true))))
        _
        (swap! resources assoc 
               :setup-command-buffer        setup-command-buffer
               :color-format-and-space      color-format-and-space
               :command-pool                command-pool
               :post-present-command-buffer post-present-command-buffer
               :queue                       queue
               :render-pass                 render-pass
               :render-command-pool         render-command-pool
               :vertices                    vertices
               :pipeline                    pipeline
               :physical-device             physical-device
               :device device
               :surface surface)
        _                           (do
                                      (GLFW/glfwSetWindowSizeCallback window window-size-callback)
                                      (GLFW/glfwShowWindow window))
        p-image-index               (MemoryUtil/memAllocInt 1)
        current-buffer              (volatile! 0)
        p-command-buffers           (MemoryUtil/memAllocPointer 1)
        p-swapchains                (MemoryUtil/memAllocLong 1)
        p-image-acquired-semaphore  (MemoryUtil/memAllocLong 1)
        p-render-complete-semaphore (MemoryUtil/memAllocLong 1)
        semaphore-create-info       (.. (VkSemaphoreCreateInfo/calloc)
                                        (sType VK11/VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO)
                                        (pNext 0)
                                        (flags vk-flags-none))
        p-wait-dst-stage-mask       (doto (MemoryUtil/memAllocInt 1)
                                      (.put 0 VK11/VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT))
        submit-info                 (.. (VkSubmitInfo/calloc)
                                        (sType VK11/VK_STRUCTURE_TYPE_SUBMIT_INFO)
                                        (pNext 0)
                                        (waitSemaphoreCount (.remaining p-image-acquired-semaphore))
                                        (pWaitSemaphores p-image-acquired-semaphore)
                                        (pWaitDstStageMask p-wait-dst-stage-mask)
                                        (pCommandBuffers p-command-buffers)
                                        (pSignalSemaphores p-render-complete-semaphore))
        present-info                (.. (VkPresentInfoKHR/calloc)
                                        (sType KHRSwapchain/VK_STRUCTURE_TYPE_PRESENT_INFO_KHR)
                                        (pNext 0)
                                        (pWaitSemaphores p-render-complete-semaphore)
                                        (swapchainCount (.remaining p-swapchains))
                                        (pSwapchains p-swapchains)
                                        (pImageIndices p-image-index)
                                        (pResults nil))]
    (try
      (while (not (GLFW/glfwWindowShouldClose window))
        (GLFW/glfwPollEvents)
        (when (:must-recreate? @resources)
          (recreate-swapchain))
        (let [err (VK11/vkCreateSemaphore device
                                          semaphore-create-info
                                          nil
                                          p-image-acquired-semaphore)
              _   (util/assert-vk-success err "Failed to create image acquired semaphore")
              err (VK11/vkCreateSemaphore device
                                          semaphore-create-info
                                          nil
                                          p-render-complete-semaphore)
              _   (util/assert-vk-success err "Failed to create render complete semaphore")
              err (KHRSwapchain/vkAcquireNextImageKHR device
                                                      (get-in @resources
                                                              [:swapchain :swapchain-handle])
                                                      uint64-max
                                                      (.get p-image-acquired-semaphore 0)
                                                      VK11/VK_NULL_HANDLE
                                                      p-image-index)
              _   (util/assert-vk-success err "Failed to acquire next swapchain image")
              _   (.put p-command-buffers 0 (aget (:render-command-buffers @resources) @current-buffer))
              err (VK11/vkQueueSubmit queue submit-info VK11/VK_NULL_HANDLE)
              _   (util/assert-vk-success err "Failed to submit render queue")
              _   (.put p-swapchains 0 (get-in @resources
                                               [:swapchain :swapchain-handle]))
              err (KHRSwapchain/vkQueuePresentKHR queue present-info)
              _   (util/assert-vk-success err "Failed to present the swapchain image")]
          (VK11/vkQueueWaitIdle queue)
          (VK11/vkDestroySemaphore device
                                   (.get p-image-acquired-semaphore 0)
                                   nil)
          (VK11/vkDestroySemaphore device
                                   (.get p-render-complete-semaphore 0)
                                   nil)
          (submit-post-present-barrier (aget (:images (:swapchain @resources))
                                             @current-buffer)
                                       post-present-command-buffer
                                       queue)))
      (finally
        (.free present-info)
        (MemoryUtil/memFree p-wait-dst-stage-mask)
        (.free submit-info)
        (MemoryUtil/memFree p-image-acquired-semaphore)
        (MemoryUtil/memFree p-render-complete-semaphore)
        (.free semaphore-create-info)
        (MemoryUtil/memFree p-swapchains)
        (MemoryUtil/memFree p-command-buffers)
        (EXTDebugReport/vkDestroyDebugReportCallbackEXT instance
                                                        debug-callback-handle
                                                        nil)
        ;; (MemoryUtil/memFree window-size-callback)
        ;; (.free key-callback)
        (GLFW/glfwDestroyWindow window)
        (GLFW/glfwTerminate)))))